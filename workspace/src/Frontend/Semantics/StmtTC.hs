module Frontend.Semantics.StmtTC where

import Frontend.Syntax
import Frontend.Semantics.Basics
import Frontend.Semantics.ExprTC
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe
-- Main Function
tychProgram :: SL -> Check ()
tychProgram (SL decls) = do
    ctx <- buildTopLevelCtx decls
    mapM_ (tychDecl ctx) decls

-- It establishes the overall context

buildTopLevelCtx :: [Loc Decl] -> Check Ctx
buildTopLevelCtx = foldl step (Right emptyCtx)
    where
        step (Left errs) _ = Left errs
        step (Right ctx) d = registerDecl ctx d

registerDecl :: Ctx -> Loc Decl -> Check Ctx
registerDecl ctx (Loc pos (Struct sname fields)) = do
    let c = ctx { currentPos = pos }
    
    -- Checks if the Struct already exists
    if M.member sname (structCtx c)
        then duplicateDecl c sname
        else do
            let fieldList = [(fname, fty) | Loc _ (Field fname fty) <- fields]
            let newStructCtx = M.insert sname fieldList (structCtx c)
            return c { structCtx = newStructCtx }

registerDecl ctx (Loc pos (Func (Generics gens) fname params mRetTy _)) = do
    let c = ctx { currentPos = pos }
    
    -- Checks if the function already exists
    if M.member fname (funcCtx c)
        then duplicateDecl c fname
        else do
            -- Strict rules for the 'main' function
            if fname == "main" && (not (null params) || isJust gens)
                then invalidMain c "The 'main' function cannot have parameters or generics."
                else do
                    -- Get generics list
                    let genList = fromMaybe [] gens
                    let rawParamTys = [ fromMaybe (TyVar pid) mTy | Loc _ (Param pid mTy) <- params ]
                        rawRetTy = case mRetTy of
                                  Just t  -> t
                                  Nothing -> case params of
                                      -- If the first parameter has no type, the return value is itself (Identity Polymorphism).
                                      (Loc _ (Param pid Nothing) : _) -> TyVar pid
                                      -- Otherwise (typed or empty parameters), it is Void.
                                      _ -> TyVoid
                    -- Apply the conversion of fake Structs to real TyVars.
                    let paramTys = map (replaceGenerics genList) rawParamTys
                        retTy = replaceGenerics genList rawRetTy

                    let sig         = (paramTys, retTy) 
                        newFuncCtx  = M.insert fname sig (funcCtx c)
                        funcType    = TyFunc paramTys retTy
                        newVarCtx   = M.insert fname funcType (varCtx c)
                    return c {funcCtx = newFuncCtx, varCtx = newVarCtx}

-- Declaration type check
tychDecl :: Ctx -> Loc Decl -> Check ()

-- Struct
tychDecl ctx (Loc pos (Struct sname fields)) = do
    let c = ctx { currentPos = pos }
    mapM_ (tychField c sname) fields

-- Function
tychDecl ctx (Loc pos (Func (Generics gens) fname params mRetTy body)) = do
    -- Building internal context
    let genList = fromMaybe [] gens
    let c = ctx { currentPos = pos, genericsCtx = genList }
    -- Search for the type of return recorded
    retTy <- case M.lookup fname (funcCtx c) of
                Just (_, r) -> Right r
                Nothing     -> undefinedFunc c fname
    innerCtx <- buildParamCtx c params
    let innerCtx' = innerCtx { returnCtx = Just retTy }
    
    -- Type Check
    void (tychBlock innerCtx' body)
    
    -- Check if void functions have a return value
    case retTy of
        TyVoid -> return ()
        _      -> checkReturns c fname body

-- Checking if the structures field is correct.
tychField :: Ctx -> ID -> Loc Field -> Check ()
tychField ctx sname (Loc pos (Field fname fty)) = do
    let c = ctx { currentPos = pos }
    case fty of
        TyStruct s -> case M.lookup s (structCtx c) of
            Nothing -> undefinedStruct c s
            Just _ -> Right ()
        _ -> Right ()

buildParamCtx :: Ctx -> [Loc Param] -> Check Ctx
buildParamCtx ctx [] = Right ctx
buildParamCtx ctx (Loc pos (Param pid mTy) : ps) = do
    let c = ctx { currentPos = pos }
    let rawTy = fromMaybe (TyVar pid) mTy
    let ty = replaceGenerics (genericsCtx c) rawTy 
    let newVarCtx = M.insert pid ty (varCtx c)
    buildParamCtx (c { varCtx = newVarCtx }) ps

-- Block
tychBlock :: Ctx -> Block -> Check Ctx
tychBlock ctx (Block stmts) = tychStmts ctx stmts

-- Statements
tychStmts :: Ctx -> [Loc Stmt] -> Check Ctx
tychStmts ctx []     = Right ctx
tychStmts ctx (s:ss) = do
    ctx' <- tychStmt ctx s
    tychStmts ctx' ss

tychStmt :: Ctx -> Loc Stmt -> Check Ctx
tychStmt ctx (Loc pos stmtCore) = do
    let c = ctx { currentPos = pos }

    case stmtCore of
        -- Var declaretion
        VarDecl vid mTy mInit ->
            -- Verify name
            if M.member vid (structCtx c) || M.member vid (funcCtx c) || M.member vid (varCtx c)
                then duplicateDecl c vid
            else
            -- Apply the conversion using the generics of the current context
                let realTy = fmap (replaceGenerics (genericsCtx c)) mTy
                 in case (mTy, mInit) of
                    (Just ty, Just initExpr) -> do
                        ti <- tychExpr c initExpr
                        _ <- checkAssign c ty ti
                        let newVarCtx = M.insert vid ty (varCtx c)
                        return c { varCtx = newVarCtx }
                    (Just ty, Nothing) -> do
                        let newVarCtx = M.insert vid ty (varCtx c)
                        return c { varCtx = newVarCtx }
                    (Nothing, Just initExpr) -> do
                        ti <- tychExpr c initExpr
                        let newVarCtx = M.insert vid ti (varCtx c)
                        return c { varCtx = newVarCtx }
                    (Nothing, Nothing) ->
                        genericError c ("Variable '" ++ vid ++ "' declared without type or initialiser")
        
        -- Return
        Return mExpr ->
            case returnCtx c of
                Nothing -> genericError c "Return statement outside function"
                Just retTy -> case mExpr of
                    Nothing ->
                        if retTy == TyVoid
                            then return c
                            else returnMismatch c retTy TyVoid
                    Just expr -> do
                        te <- tychExpr c expr
                        _ <- checkAssign c retTy te
                        return c

        -- Print
        Print expr -> do
            void (tychExpr c expr)
            return c

        -- Scan
        Scan expr -> do
            void (tychValue c expr)
            return c

        -- Break
        Break -> if inLoop c 
                    then return c 
                    else breakOutside c

        -- Continue
        Continue -> if inLoop c 
                       then return c 
                       else continueOutside c

        -- If / Elif* / Else
        IF cond thenBlk elifs mElse -> do
            tc <- tychExpr c cond
            _ <- requireEqual c TyBool tc
            void (tychBlock c thenBlk)
            mapM_ (tychElif c) elifs
            case mElse of
                Nothing         -> return ()
                Just elseBlk    -> void (tychBlock c elseBlk)
            return c
        
        -- While
        While cond body -> do
            tc <- tychExpr c cond
            _ <- requireEqual c TyBool tc
            let loopCtx = c { inLoop = True }
            void (tychBlock loopCtx body)
            return c
        
        -- For
        For initStmt condE stepE body -> do
            ctxComI <- tychStmt c (Loc pos initStmt)
            tc <- tychExpr ctxComI condE
            _ <- requireEqual ctxComI TyBool tc
            _ <- tychExpr ctxComI stepE
            let loopCtx = ctxComI { inLoop = True }
            void (tychBlock loopCtx body)
            return c
        
        -- Isolated Expr
        Exp expr -> do
            void (tychExpr c expr)
            return c

  where
    tychElif c (e, blk) = do
        te <- tychExpr c e
        _ <- requireEqual c TyBool te
        void (tychBlock c blk)

-- Return
checkReturns :: Ctx -> ID -> Block -> Check ()
checkReturns ctx fname (Block stmts)
    | blockAlwaysReturns stmts = Right ()
    | otherwise = missingReturn ctx fname

blockAlwaysReturns :: [Loc Stmt] -> Bool
blockAlwaysReturns [] = False
blockAlwaysReturns stmts = any stmtAlwaysReturns stmts

stmtAlwaysReturns :: Loc Stmt -> Bool
stmtAlwaysReturns (Loc _ (Return _)) = True
stmtAlwaysReturns (Loc _ (IF _ (Block thenS) elifs (Just (Block elseS)))) =
    blockAlwaysReturns thenS
    && all (\(_, Block bs) -> blockAlwaysReturns bs) elifs
    && blockAlwaysReturns elseS
stmtAlwaysReturns _ = False
