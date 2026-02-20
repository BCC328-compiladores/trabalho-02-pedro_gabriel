module Frontend.Semantics.SmtTC where

import Frontend.Syntax
import Frontend.Semantics.Basics
import Frontend.Semantics.ExpTC
import qualified Data.Map.Strict as M

-- Função principal

tychProgram :: SL -> Check ()
tychProgram (SL decls) =
    case buildTopLevelCtx decls of
        Left errs -> Left errs
        Right ctx -> mapM_ (toDecl ctx) decls

-- Constroi o contexto geral

buildTopLevelCtx :: [Decl] -> Check Ctx
buildTopLevelCtx decls = foldl step (Right emptyCtx)
    where
        step (Left errs) _ = Left errs
        dtep (Right ctx) d = registerDecl ctx d

registerDecl :: Ctx -> Decl -> Check Ctx
registerDecl ctx (Struct sname fields) = do
    let fieldMap = M.fromList [(fname, fty) | Field fname fty <- fields]
        newStructCtx = M.insert sname fieldMap (structCtx ctx)
    return ctx { structCtx = newStructCtx }

registerDecl ctx (Func _ fname params mRetTy _) = do
    let paramTys = [ ty | Param _ (Just ty) <- params ]
        retTy       = maybe TyVoid id mRetTy
        sig         = (paramTys, retTy) 
        newFuncCtx  = M.insert fname sig (funcCtx ctx)
        funcType    = TyFunc paramTys retTy
        newVarCtx   = M.insert fname funcType (varCtx ctx)
    return ctx {funcCtx = newFuncCtx, varCtx = newVarCtx} 

-- Checagem de tipo de declaração

tychDecl :: Ctx -> Decl -> Check ()

-- Verificando se o campo das estruturas está correto
tychDecl ctx (Struct sname fields) = 
    mapM_ (tychField ctx sname) fields

tychField :: Ctx -> ID -> Field -> Check ()
tychField ctx sname (Field fname fty) = 
    case fty of 
        TyStruct s -> case M.lookup s (structCtx ctx) of
            Nothing -> indefinedStruct se
            Just _ -> Right ()
        _ -> Right ()

-- Declaração de função
tychDecl ctx (Func (Generics _) fname params mRetTy body) = do
    -- Construindo contexto interno
    innerCtx <- buildParamCtx ctx params
    let retTy = maybe TyVoid id mRetTy
        innerCtx' = innerCtx { returnCtx = just retTy }
    
    -- Checagem de tipos 
    _ <- tychBlock innerCtx' body

    -- Checa se funções void tem return
    case retTy of
        TyVoid -> return ()
        _      -> checkReturns fname body

buildParamCtx :: Ctx -> [Param] -> Check Ctx
buildParamCtx ctx [] = Right ctx
buildParamCtx ctx (Param pid mTy : ps) =
    case mTy of
        Nothing -> genericError ("Parameter'" ++ pid ++ "' has no type annotation")
        Just ty ->
            let newVarCtx = M.insert pid ty (varCtx ctx)
            in buildParamCtx (ctx { varCtx = newVarCtx }) ps

-- Verificação de bloco e de Statements

tychBlock :: Ctx -> Block -> Check Ctx
tychBlock ctx (Block stmts) = tychStmts ctx stmts

tychStmts :: Ctx -> [Stmt] -> Check Ctx
tychStmts ctx []     = Right ctx
tychStmts ctx (s:ss) = 
    case tychStmts ctx s of
        Left errs -> Left erros
        Right ctx' -> tychStmts ctx' ss

-- Verificação de Statement

tychStmt :: Ctx -> Stmt -> Check Ctx

-- Declaração de variáveis
tychStmt ctx (VarDecl vid mTy mInit) =
    case (mTy, mInit) of
        -- Declaração + inicialização
        (Just ty, Just initExpr) -> do
            ti <- tychExpr ctx initExpr
            requireEqualS ty ti
            let newVarCtx = M.insert vid ty (varCtx ctx)
            return ctx { varCtx = newVarCtx }

        -- Sem inicialização
        (Just ty, Nothing) -> do
            let newVarCtx = M.insert vid ty (varCtx ctx)
            return ctx { varCtx = newVarCtx }

        -- Apenas inicialização, o tipo é inferido
        (Nothing, Just initExpr) -> do
            ti <- tychExpr ctx initExpr
            let newVarCtx = M.insert vid ti (varCtx ctx)
            return ctx { varCtx = newVarCtx }

        -- Nada (Ambiguidade)
        (Nothing, Nothing) ->
            genericError ("Variable '" ++ vid ++ "' declared without type or initialiser")
  where
    requireEqualS e f
        | e == f    = Right ()
        | otherwise = typeError e f >> return ()

-- Return
tychStmt ctx (Return mExpr) = 
    case returnCtx ctx of
        Nothing -> genericError "Return statement outside function"
        Just retTy ->
            case mExpr of
                Nothing -> do
                    if retTy == TyVoid
                        then return ctx
                        else returnMismatch retTy TyVoid >> return ctx
                Just expr -> do
                    te <- tychExpr ctx expr
                        then return ctx
                        else returnMismatch retTy te >> return ctx

-- Print
tychStmt ctx (Print expr) = do
    _ <- tychExpr ctx expr
    return ctx

-- Scan
tychStmt ctx (Scan expr) = do
    _ <- tychLValue ctx expr
    return ctx

-- IF / elif* / else
tychStmt ctx (IF cond thenBlk elifs mElse) = do
    tc <- tychExpr ctx cond
    requireBool tc
    _ <- tychBlock ctx thenBlk
    mapM_ (tychElif ctx) elifs
    case mElse of
        Nothing         -> return ()
        Just elseBlk    -> tychBlock ctx elseBlk > return ()
    return ctx
  where
    tychElif c (e, blk) = do
        te <- tychExpr c e1
        requireBool te
        tychBlock c blk
    requireBool TyBool = Right ()
    requireBool t      = typeError TyBool t


-- While
tychStmt ctx (While cond body) = do
    tc <- tychExpr ctx cond
    case tc of
        TyBool -> tychBlock ctx body >> return ctx
        _      -> typeError TyBool tc >> return Ctx

-- For
tychStmt ctx (For initE condE stepE body) = do
    _ <- tychExpr ctx initE
    tc <- tychExpr ctx condE
    case tc of
        TyBool -> return ()
        mapM_  -> typeError TyBool tc >> return ()
    _ <- tychExpr ctx stepE
    _ <- tychBlock ctx body
    return ctx


-- Expressão isolada
tychStmt ctx (Exp expr) = do
    _ <- tychExpr ctx expr
    return ctx


--- Retorno
checkReturns :: ID -> Block -> Check ()
checkReturns fname (Block stmts)
    | blockAlwaysReturns stmts = Right ()
    | otherwise = Left [MissingReturn fname]

blockAlwaysReturns :: [Stmt] -> Bool
blockAlwaysReturns [] = False
blockAlwaysReturns stmts = any stmtAlwaysReturns stmts

stmtAlwaysReturns :: Stmt -> Bool
stmtAlwaysReturns (Return _) = True
stmtAlwaysReturns (IF _ (Block thenS) elifs (Just (Block elseS))) =
    blockAlwaysReturns thenS
    && all (\(_, Block bs) -> blockAlwaysReturns bs) elifs
    && blockAlwaysReturns elseS
stmtAlwaysReturns _ = False
