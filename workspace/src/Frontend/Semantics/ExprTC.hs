module Frontend.Semantics.ExprTC where

import Frontend.Syntax
import Frontend.Semantics.Basics

import qualified Data.Map.Strict as M
import Control.Monad 

--- Binary operation helpers 

-- Sum and Concatenate Operation Helper
resolveAdd :: Ctx -> Type -> Type -> Check Type
resolveAdd _ TyInt TyInt       = Right TyInt
resolveAdd _ TyFloat TyFloat   = Right TyFloat
resolveAdd _ TyInt TyFloat     = Right TyFloat
resolveAdd _ TyFloat TyInt     = Right TyFloat
resolveAdd _ TyString TyString = Right TyString
resolveAdd _ TyString TyInt    = Right TyString
resolveAdd _ TyInt TyString    = Right TyString
resolveAdd _ TyString TyFloat  = Right TyString
resolveAdd _ TyFloat TyString  = Right TyString
resolveAdd ctx t1 t2 = genericError ctx ("Cannot sum or concatenate " ++ show t1 ++ " and " ++ show t2)

-- Helper for -, *, /
resolveMath :: Ctx -> Type -> Type -> Check Type
resolveMath _ TyInt TyInt     = Right TyInt
resolveMath _ TyFloat TyFloat = Right TyFloat
resolveMath _ TyInt TyFloat   = Right TyFloat
resolveMath _ TyFloat TyInt   = Right TyFloat
resolveMath ctx t1 t2 = genericError ctx ("Cannot subtract, multiply, or divide " ++ show t1 ++ " and " ++ show t2)

-- Helper for ==, !=, <, >, <=, >=
resolveCmp :: Ctx -> Type -> Type -> Check Type
resolveCmp _ TyInt TyInt       = Right TyBool
resolveCmp _ TyFloat TyFloat   = Right TyBool
resolveCmp _ TyInt TyFloat     = Right TyBool
resolveCmp _ TyFloat TyInt     = Right TyBool
resolveCmp _ TyString TyString = Right TyBool
resolveCmp ctx t1 t2 = genericError ctx ("Cannot compare " ++ show t1 ++ " and " ++ show t2)

-- Helper for &&, ||
resolveLogical :: Ctx -> Type -> Type -> Check Type
resolveLogical ctx t1 t2 = do
    _ <- requireEqual ctx TyBool t1
    _ <- requireEqual ctx TyBool t2
    return TyBool

tychBinOp :: Ctx -> Expr -> Expr -> (Ctx -> Type -> Type -> Check Type) -> Check Type
tychBinOp ctx e1 e2 solver =
    case bothCheck (tychExpr ctx e1) (tychExpr ctx e2) of
        Left errs -> Left errs
        Right (t1, t2) -> solver ctx t1 t2

-- Evaluation of Expressions
tychExpr :: Ctx -> Expr -> Check Type

-- Literals
tychExpr _ (LitInt _)    = Right TyInt
tychExpr _ (LitFloat _)  = Right TyFloat
tychExpr _ (LitString _) = Right TyString
tychExpr _ (LitBool _)   = Right TyBool

tychExpr ctx (LitArray []) = genericError ctx "Cannot infer type of empty array literal"
tychExpr ctx (LitArray elements@(e:es)) = do
    t <- tychExpr ctx e
    mapM_ (tychExpr ctx >=> requireEqual ctx t) es
    let size = length elements
    return (TyArray t (Just (LitInt size)))

-- Variable
tychExpr ctx (Var v) = lookupVar ctx v

-- Parentheses
tychExpr ctx (Paren e) = tychExpr ctx e

-- Assignment 
tychExpr ctx (lhs := rhs) = do
    tl <- tychValue ctx lhs 
    tr <- tychExpr ctx rhs
    _ <- requireEqual ctx tl tr
    return TyVoid

-- Logical 
tychExpr ctx (e1 :||: e2) = tychBinOp ctx e1 e2 resolveLogical
tychExpr ctx (e1 :&&: e2) = tychBinOp ctx e1 e2 resolveLogical

-- Equality
tychExpr ctx (e1 :==: e2) = tychBinOp ctx e1 e2 resolveCmp
tychExpr ctx (e1 :!=: e2) = tychBinOp ctx e1 e2 resolveCmp

-- Comparison
tychExpr ctx (e1 :<: e2)  = tychBinOp ctx e1 e2 resolveCmp
tychExpr ctx (e1 :<=: e2) = tychBinOp ctx e1 e2 resolveCmp
tychExpr ctx (e1 :>: e2)  = tychBinOp ctx e1 e2 resolveCmp
tychExpr ctx (e1 :>=: e2) = tychBinOp ctx e1 e2 resolveCmp

-- Arithmetical
tychExpr ctx (e1 :+: e2) = tychBinOp ctx e1 e2 resolveAdd
tychExpr ctx (e1 :-: e2) = tychBinOp ctx e1 e2 resolveMath
tychExpr ctx (e1 :*: e2) = tychBinOp ctx e1 e2 resolveMath
tychExpr ctx (e1 :/: e2) = tychBinOp ctx e1 e2 resolveMath

-- Unary
tychExpr ctx (Not e) = do
    t <- tychExpr ctx e 
    requireEqual ctx TyBool t

tychExpr ctx (Neg e) = do
    t <- tychExpr ctx e
    requireNumeric ctx t

tychExpr ctx (PostInc e) = do
    t <- tychValue ctx e
    requireEqual ctx TyInt t 

tychExpr ctx (PostDec e) = do
    t <- tychValue ctx e
    requireEqual ctx TyInt t

-- Struct Field Access
tychExpr ctx (e :.: field) = do
    t <- tychExpr ctx e
    case t of
        TyStruct sname ->
            case M.lookup sname (structCtx ctx) of
                Nothing -> undefinedStruct ctx sname
                Just fields ->
                    case lookup field fields of
                        Nothing -> undefinedField ctx sname field
                        Just ft -> Right ft
        TyArray _ _ -> 
            if field == "size" 
            then Right TyInt 
            else genericError ctx "Arrays only have '.size' property"
        _ -> notAStruct ctx t

-- Acesso ao array
tychExpr ctx (arr :@: idx) = do
    ta <- tychExpr ctx arr
    ti <- tychExpr ctx idx
    case ti of
        TyInt -> return ()
        _ -> void (typeError ctx TyInt ti)
    case ta of
        TyArray elemTy _    -> return elemTy
        _                   -> notAnArray ctx ta

-- Function Call
tychExpr ctx (FuncCall fexpr args) = do
    tf <- tychExpr ctx fexpr
    case tf of 
        TyFunc paramTys retTy -> do
            let nExpected = length paramTys
                nFound    = length args
            if nExpected /= nFound
                then arityMismatch ctx "<function>" nExpected nFound
                else do
                    argTys <- mapM (tychExpr ctx) args
                    
                    -- Inference of Generics
                    let buildGenericsMap m (TyVar tName, vTy) = 
                            case M.lookup tName m of
                                Just expected -> if expected == vTy then Right m 
                                                 else genericError ctx ("Generics conflict: '" ++ tName ++ "' expected " ++ show expected ++ " but got " ++ show vTy)
                                Nothing -> Right (M.insert tName vTy m)
                        buildGenericsMap m (TyArray t1 _, TyArray t2 _) =
                            buildGenericsMap m (t1, t2)
                        buildGenericsMap m (TyFunc p1 r1, TyFunc p2 r2) = do
                            if length p1 /= length p2 then Right m else do
                                m1 <- foldM buildGenericsMap m (zip p1 p2)
                                buildGenericsMap m1 (r1, r2)

                        buildGenericsMap m _ = Right m

                    genMap <- foldM buildGenericsMap M.empty (zip paramTys argTys)

                    -- Resolve os tipos usando o mapa extraído
                    let resolveTy m (TyVar n) = case M.lookup n m of { Just t -> t; Nothing -> TyVar n }
                        resolveTy m (TyArray t size) = TyArray (resolveTy m t) size
                        resolveTy m (TyFunc ps ret) = TyFunc (map (resolveTy m) ps) (resolveTy m ret)
                        resolveTy _ t = t

                    let resolvedParams = map (resolveTy genMap) paramTys
                    let resolvedRet    = resolveTy genMap retTy

                    zipWithM_ (checkAssign ctx) resolvedParams argTys
                    return resolvedRet
        _ -> notCallable ctx tf


-- Object Creation
tychExpr ctx (NewObj sname args) = do
    case M.lookup sname (structCtx ctx) of
        Nothing -> undefinedStruct ctx sname
        Just fields -> do
            if null args
               then return (TyStruct sname)
               else do
                    let fieldTys = map snd fields
                        nExp    = length fieldTys
                        nFound  = length args
                    if nExp /= nFound
                        then arityMismatch ctx sname nExp nFound
                        else do
                            argTys <- mapM (tychExpr ctx) args
                            zipWithM_ (requireEqual ctx) fieldTys argTys
                            return (TyStruct sname)


-- Array Creatio
tychExpr ctx (NewArray elemTy dimExprs) = do
    mapM_ (tychExpr ctx >=> requireEqual ctx TyInt) dimExprs
    
    -- Transform TyStruct "b" into TyVar "b"
    let realElemTy = replaceGenerics (genericsCtx ctx) elemTy
    
    let buildArrayType t 0 = t
        buildArrayType t n = TyArray (buildArrayType t (n - 1)) Nothing
        
    return (buildArrayType realElemTy (length dimExprs))

-- Value Checker
tychValue :: Ctx -> Expr -> Check Type
tychValue ctx (Var v)          = lookupVar ctx v
tychValue ctx (e :.: field)    = tychExpr ctx (e :.: field) 
tychValue ctx (arr :@: idx)    = tychExpr ctx (arr :@: idx)
tychValue ctx (Paren e)        = tychValue ctx e
tychValue ctx e                = notAssignable ctx e
