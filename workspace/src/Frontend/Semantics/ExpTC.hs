module Frontend.Semantics.ExpTC where

import Frontend.Parser.Syntax
import Frontend.Semantics.Basics
import qualified Data.Map.Strict as M

--- Helpers Genericos 

-- Busca uma variável no contexto
lookupVar :: Ctx -> ID -> Check Type
lookupVar ctx v =
    case M.lookup v (varCtx ctx) of
        Just ty -> Right ty
        Nothing -> undefinedVar v

-- Checa se um tipo é numerico (int ou float)
requireNumeric :: Type -> Check Type
requireNumeric TyInt   = Right TyInt
requireNumeric TyFloat = Right TyFloat
requireNumeric t       = typeError TyInt t

-- Checa igualdade entre dois tipos 
requireEqual :: Expected -> Found -> Check Type
requireEqual e f
    | e == f    = Right e
    | otherwise = typeError e f

--- Helpers para operadores binários
-- Ambos os operandos devem ser do mesmo tipo (opType), resutando em resType
tychBinOp :: Ctx -> Expr -> Expr -> Type -> Type -> Check Type
tychBinOp ctx e1 e2 opType resType =
    case bothCheck (tychExpr ctx e1)(tychExpr ctx e2) of
        Left errs       -> Left errs
        Right (t1,t2)   -> 
            case (requireEqual opTy t1, requireEqual opTy t2) of
                (Left e, Left f) -> Left (e ++ f)
                (Left e, _)      -> Left e
                (_, Left f)      -> Left f
                _                -> Right resTy            

tychArith :: Ctx -> Expr -> Expr -> Check Type
tychArith ctx e1 e2 =
    case bothCheck (tychExpr ctx e1) (tychExpr ctx e2) of
        Left errs      -> Left errs
        Right (t1, t2) ->
            case (requireNumeric t1, requireNumeric t2) of
                (Left e, Left f) -> Left (e ++ f)
                (Left e, _)      -> Left e
                (_, Left f)      -> Left f
                (Right nt1, Right nt2) ->
                    if nt1 == nt2
                    then Right nt1
                    else typeError nt1 nt2

tychCmp :: Ctx -> Expr -> Expr -> Check Type
tychCmp ctx e1 e2 =
    case tychArith ctx e1 e2 of
        Left errs -> Left errs
        Right _   -> Right TyBool


tychExpr :: Ctx -> Expr -> Check Type

-- Literais
tychExpr _ (LitInt _)    = Right TyInt
tychExpr _ (LitFloat _)  = Right TyFloat
tychExpr _ (LitString _) = Right TyString
tychExpr _ (LitBool _)   = Right TyBool

tychExpr ctx (LitArray []) =
    genericError "Cannot infer type of empty array literal"
tychExpr ctx (LitArray (e:es)) = do
    t <- tychExpr ctx e
    mapM_ (\ei -> tychExpr ctx ei >>= requireEqual t) es
    return (TyArray t Nothing)

-- Variavel
tychExpr ctx (Var v) = lookupVar ctx v

-- Expressão entre parenteses
tychExpr ctx (Paren e) = tychExpr ctx e

-- Atribuição 
tychExpr ctx (lhs := rhs) = do
    tl <- tychValue ctx lhs
    tr <- tychExpr ctx rhs
    requireEqual tl tr
    return tl

-- Logical 
tychExpr ctx (e1 :||: e2) = tychBinOp ctx e1 e2 TyBool TyBool
tychExpr ctx (e1 :&&: e2) = tychBinOp ctx e1 e2 TyBool TyBool

-- Igualdade
tychExpr ctx (e1 :==: e2) =
    case bothCheck (tychExpr ctx e1)(tychExpr ctx e2) of
        Left errs       -> Left errs
        Right (t1,t2)   -> requireEqual t1 t2 >> return TyBool

tychExpr ctx (e1 :!=: e2) =
    case bothCheck (tychExpr ctx e1)(tychExpr ctx e2) of
        Left errs       -> Left errs
        Right (t1,t2)   -> requireEqual t1 t2 >> return TyBool

-- Comparação
tychExpr ctx (e1 :<: e2) = tychCmp ctx e1 e2
tychExpr ctx (e1 :<=: e2) = tychCmp ctx e1 e2
tychExpr ctx (e1 :>: e2) = tychCmp ctx e1 e2
tychExpr ctx (e1 :>=: e2) = tychCmp ctx e1 e2

-- Aritméticos
tychExpr ctx (e1 :+: e2) = tychArith ctx e1 e2
tychExpr ctx (e1 :-: e2) = tychArith ctx e1 e
tychExpr ctx (e1 :*: e2) = tychArith ctx e1 e2
tychExpr ctx (e1 :/: e2) = tychArith ctx e1 e

-- Unarios
tychExpr ctx (Not e) = do
    t <- tychExpr ctx e 
    requireEqual TyBool t
    return TyBool

tychExpr ctx (Neg e) = do
    t <- tychExpr ctx e
    requireNumeric t

tychExpr ctx (PostInc e) = do
    t <- tychLValue ctx e
    requireNumeric t

tychExpr ctx (PostInc e) = do
    t <- tychLValue ctx e
    requireNumeric t

-- Acesso ao campo de estrutura
tychExpr ctx (e :.: field) = do
    t <- tychExpr ctx e
    case t of
        TyStruc sname ->
            case M.lookup sname (structCtx ctx) of
                Nothing -> undefinedStruct sname
                Just fields ->
                    case M.lookup field fields of
                        Nothing -> undefinedField sname field
                        Just ft -> Right ft
        _ -> notAStruct t

-- Acesso ao array
tychExpr ctx (arr :@: idx) = do
    ta <- tychExpr ctx arr
    ti <- tychExpr ctx idx
    case ti of
        TyInt -> return ()
        _     -> typeError TyInt ti >> return ()
    case ta of
        TyArray elemTy _    -> return elemTy
        _                   -> notAnArray ta

-- Chamada da função
tychExpr ctx (FuncCall fexpr args) = do
    tf <- tychExpr ctx fexpr
    case tf of 
        TyFunc paramTys retTy -> do
            let nExpected = length paramTys
                nFound    = length args
            if nExpected /= nFound
                then arityMismatch "<lambda>" nExpected nFound
                else do
                    argTys <- mapM (tychExpr ctx) args
                    sequence_ (zipWith requireEqual paramTys argTys)
                    return retTy
        _ -> notCallable tf


-- Construção da estrutura
tychExpr ctx (NewObj sname args) = do
    case M.lookup sname (structCtx ctx) of
        Nothing -> undefinedStruct sname
        Just fields -> do
            let fieldTys = M.elems fields
                nExp    = length fieldTys
                nFound  = length args
            if nExp /= nFound
                then arityMismatch sname nExp nFound
                else do
                    argTys <- mapM (tychExpr ctx) args
                    sequence_ (zipWith requireEqual fieldTys argTys)
                    return (TyStruct sname)


-- Construção do array (Verifica se as dimensões são inteiros)
tychExpr ctx (NewArray elemTy dimExprs) = do
    mapM_ (\d -> tychExpr ctx d >>= requireEqual TyInt) dimExprs
    return (TyArray elemTy Nothing)

-- LValue Checker
tychLValue :: Ctx -> Expr -> Check Type
tychLValue ctx (Var v)          = lookupVar ctx v
tychLValue ctx (e :.: field)    = tychExpr ctx (e :.: field) 
tychLValue ctx (arr :@: idx)    = tychExpr ctx (arr :@: idx)
tychLValue ctx (Paren e)        = tychLValue ctx e
tychLValue _ e                  = Left [NotAssignable e]
