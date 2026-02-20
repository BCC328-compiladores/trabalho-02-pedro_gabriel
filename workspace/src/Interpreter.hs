{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Interpreter where

import Data.IORef
import qualified Data.Map as M
import Control.Monad (foldM, zipWithM, unless)
import Data.Sequence (Seq(Empty), empty)

import Frontend.Syntax

type Env = M.Map ID (IORef Value)

data Globals = Globals {
    gStructs :: M.Map ID [(ID, Type)],
    gFuncs   :: M.Map ID Decl
}

-- ==========================================
-- Aux Operations
-- ==========================================

-- Arithmetical
(.+.) :: Value -> Value -> Value
(ValInt a)    .+. (ValInt b)    = ValInt $ a + b
(ValFloat a)  .+. (ValFloat b)  = ValFloat $ a + b
(ValFloat a)  .+. (ValInt b)    = ValFloat $ a + fromIntegral b
(ValInt a)    .+. (ValFloat b)  = ValFloat $ fromIntegral a + b
(ValString a) .+. (ValString b) = ValString $ a ++ b
(ValString a) .+. (ValInt b)    = ValString $ a ++ show b
(ValInt a)    .+. (ValString b) = ValString $ show a ++ b
(ValString a) .+. (ValFloat b)  = ValString $ a ++ show b
(ValFloat a)  .+. (ValString b) = ValString $ show a ++ b
_             .+. _             = error "Type error"

(.-.) :: Value -> Value -> Value
(ValInt a)   .-. (ValInt b)   = ValInt $ a - b
(ValFloat a) .-. (ValFloat b) = ValFloat $ a - b
(ValFloat a) .-. (ValInt b)   = ValFloat $ a - fromIntegral b
(ValInt a)   .-. (ValFloat b) = ValFloat $ fromIntegral a - b
_            .-. _            = error "Type error"

(.*.) :: Value -> Value -> Value
(ValInt a)   .*. (ValInt b)   = ValInt $ a * b
(ValFloat a) .*. (ValFloat b) = ValFloat $ a * b
(ValFloat a) .*. (ValInt b)   = ValFloat $ a * fromIntegral b
(ValInt a)   .*. (ValFloat b) = ValFloat $ fromIntegral a * b
_            .*. _            = error "Type error"

(./.) :: Value -> Value -> Value
(ValInt a)   ./. (ValInt b)   = ValInt $ a `div` b
(ValFloat a) ./. (ValFloat b) = ValFloat $ a / b
(ValFloat a) ./. (ValInt b)   = ValFloat $ a / fromIntegral b
(ValInt a)   ./. (ValFloat b) = ValFloat $ fromIntegral a / b
_            ./. _            = error "Type error"

-- Comparison
-- not == for !=
(.==.) :: Value -> Value -> Value
(ValInt a)    .==. (ValInt b)    = ValBool $ a == b
(ValFloat a)  .==. (ValFloat b)  = ValBool $ a == b
(ValFloat a)  .==. (ValInt b)    = ValBool $ a == fromIntegral b
(ValInt a)    .==. (ValFloat b)  = ValBool $ fromIntegral a == b
(ValString a) .==. (ValString b) = ValBool $ a == b
_             .==. _             = error "Type error"
 
-- not < for >=
(.<.) :: Value -> Value -> Value
(ValInt a)    .<. (ValInt b)    = ValBool $ a < b
(ValFloat a)  .<. (ValFloat b)  = ValBool $ a < b
(ValFloat a)  .<. (ValInt b)    = ValBool $ a < fromIntegral b
(ValInt a)    .<. (ValFloat b)  = ValBool $ fromIntegral a < b
(ValString a) .<. (ValString b) = ValBool $ a < b
_             .<. _             = error "Type error"

-- not > for <=
(.>.) :: Value -> Value -> Value
(ValInt a)    .>. (ValInt b)    = ValBool $ a > b
(ValFloat a)  .>. (ValFloat b)  = ValBool $ a > b
(ValFloat a)  .>. (ValInt b)    = ValBool $ a > fromIntegral b
(ValInt a)    .>. (ValFloat b)  = ValBool $ fromIntegral a > b
(ValString a) .>. (ValString b) = ValBool $ a > b
_             .>. _             = error "Type error"

(.||.) :: Value -> Value -> Value
(ValBool a) .||. (ValBool b) = ValBool (a || b)
_           .||. _           = error "Type error"

(.&&.) :: Value -> Value -> Value
(ValBool a) .&&. (ValBool b) = ValBool (a && b)
_           .&&. _           = error "Type error"

-- Applies binary options
interpBinOp :: (Value -> Value -> Value) -> Expr -> Expr -> Globals -> Env -> IO Value
interpBinOp f e1 e2 g env 
  = do
       v1 <- interpExpr g env e1
       v2 <- interpExpr g env e2
       return (f v1 v2)

-- Unary
opNot :: Value -> Value
opNot (ValBool b) = ValBool (not b)
opNot _           = error "Type error"

opNeg :: Value -> Value
opNeg (ValInt n)   = ValInt (-n)
opNeg (ValFloat n) = ValFloat (-n)
opNeg _            = error "Type error"

interpUnary :: (Value -> Value) -> Expr -> Globals -> Env -> IO Value
interpUnary f expr g env = do
    val <- interpExpr g env expr
    return (f val)

-- ==========================================
-- Expressions Interpreter
-- ==========================================

interpExpr :: Globals -> Env -> Expr -> IO Value

-- Literals
interpExpr _ _ (LitInt a)    = return $ ValInt a
interpExpr _ _ (LitFloat a)  = return $ ValFloat a
interpExpr _ _ (LitString a) = return $ ValString a
interpExpr _ _ (LitBool a)   = return $ ValBool a
interpExpr g env (LitArray exprs) = do
    values <- mapM (interpExpr g env) exprs
    ref <- newIORef values
    let typeTag = case values of
            (ValInt _):_   -> TyInt
            (ValFloat _):_ -> TyFloat
            (ValString _):_-> TyString
            (ValBool _):_  -> TyBool
            _              -> TyVoid
    return $ ValArray typeTag ref

-- Vars
interpExpr g env (Var id) = do
    case M.lookup id env of
        Just ref -> readIORef ref
        Nothing  ->
            case M.lookup id (gFuncs g) of
                Just (Func _ _ params retType body) -> 
                    return $ ValClosure params retType body M.empty
                Nothing -> error $ "Variable or Function not defined: " ++ id

-- Assignment
interpExpr g env (lhs := rhs) = do
    val <- interpExpr g env rhs
    writeValue g env lhs val
    return ValVoid -- := is a Expr and has no change

-- Operations
interpExpr g env (e1 :+: e2)  = interpBinOp (.+.) e1 e2 g env             
interpExpr g env (e1 :-: e2)  = interpBinOp (.-.) e1 e2 g env            
interpExpr g env (e1 :*: e2)  = interpBinOp (.*.) e1 e2 g env            
interpExpr g env (e1 :/: e2)  = interpBinOp (./.) e1 e2 g env            

interpExpr g env (e1 :==: e2) = interpBinOp (.==.) e1 e2 g env           
interpExpr g env (e1 :!=: e2) = (\(ValBool b) -> ValBool (not b)) <$> interpBinOp (.==.) e1 e2 g env 
interpExpr g env (e1 :<: e2)  = interpBinOp (.<.) e1 e2 g env           
interpExpr g env (e1 :>=: e2) = (\(ValBool b) -> ValBool (not b)) <$> interpBinOp (.<.) e1 e2 g env           
interpExpr g env (e1 :>: e2)  = interpBinOp (.>.) e1 e2 g env            
interpExpr g env (e1 :<=: e2) = (\(ValBool b) -> ValBool (not b)) <$> interpBinOp (.>.) e1 e2 g env            

interpExpr g env (e1 :&&: e2) = interpBinOp (.&&.) e1 e2 g env          
interpExpr g env (e1 :||: e2) = interpBinOp (.||.) e1 e2 g env       

interpExpr g env (Not e) = interpUnary opNot e g env
interpExpr g env (Neg e) = interpUnary opNeg e g env

interpExpr g env (PostInc value) = handleIncDec g env value 1
interpExpr g env (PostDec value) = handleIncDec g env value (-1)

-- Struct recursive acess
interpExpr g env (objExpr :.: field) = do
    objVal <- interpExpr g env objExpr
    case objVal of
        ValStruct id fieldsMap -> 
            case M.lookup field fieldsMap of
                Just ref -> readIORef ref
                Nothing -> error $ "Field '" ++ field ++ "' does not exist in '" ++ id ++ "'."
        ValArray _ ref -> 
            if field == "size" 
            then do
                list <- readIORef ref
                return $ ValInt $ length list -- Return Array Size
            else 
                error $ "Field '" ++ field ++ "' does not exist in Array. Use '.size'."
        _ -> error "The attempt to access the field is invalid."

-- Array recursive acess
interpExpr g env (arrExpr :@: idxExpr) = do
    arrVal <- interpExpr g env arrExpr
    idxVal <- interpExpr g env idxExpr
    case (arrVal, idxVal) of
        (ValArray _ ref, ValInt i) -> do
            list <- readIORef ref
            if i >= 0 && i < length list 
                then return (list !! i)
                else error $ "Index out of bounds: " ++ show i
        _ -> error "Invalid access to array."

-- Create Obj 
interpExpr g env (NewObj structName args) = do
    argVals <- mapM (interpExpr g env) args
    -- Search for global definition
    case M.lookup structName (gStructs g) of
        Nothing -> error $ "Struct '" ++ structName ++ "' does not exist."
        Just fieldDefs -> do
            finalVals <- if null argVals
                then mapM (\(_, t) -> defaultValue t) fieldDefs -- If empty args add defaults
                else do
                    if length fieldDefs /= length argVals
                        then error $ "Incorrect number of arguments for the struct '" ++ structName ++ "'."
                        else do
                            -- Type Validation
                            let pairs = zip fieldDefs argVals
                            mapM_ (\((fieldName, fieldType), val) -> 
                                unless (checkType fieldType val) $
                                    error $ "Type error in the field '" ++ fieldName ++ "' in the struct '" ++ structName ++ 
                                            "'. Expected " ++ show fieldType ++ " but received: " ++ show val
                                ) pairs
                            return argVals

            -- Create memory ref
            refs <- mapM newIORef finalVals
            let fieldsMap = M.fromList $ zip [n | (n,_) <- fieldDefs] refs
            return $ ValStruct structName fieldsMap


-- Create Array 
interpExpr g env (NewArray baseType dimsExpr) = do
    dimValues <- mapM (interpExpr g env) dimsExpr
    -- Check if sizes is valid 
    sizes <- mapM (\val -> case val of
        ValInt i -> if i >= 0 
                    then return i 
                    else error "The Array size cannot be negative."
        _ -> error "The array size must be of type 'int'."
        ) dimValues
    if null sizes 
        then error "New Array requires at least one dimension."
        else createArrayRecursive baseType sizes

  where
    -- Auxiliary function to create Array with default values
    createArrayRecursive :: Type -> [Int] -> IO Value
    createArrayRecursive t [size] = do
        def <- defaultValue t 
        vals <- sequence <$> replicate size $ return def 
        ref <- newIORef vals
        return $ ValArray t ref
    createArrayRecursive t (size:rest) = do
        innerArrays <- sequence <$> replicate size $ createArrayRecursive t rest
        ref <- newIORef innerArrays
        let innerType = calcArrayType t (length rest)
        return $ ValArray innerType ref 

    -- Auxiliary function for calculating nested types
    calcArrayType :: Type -> Int -> Type
    calcArrayType t 0 = t
    calcArrayType t depth = TyArray (calcArrayType t (depth - 1)) Nothing

-- Function Call
interpExpr g env (FuncCall funcExpr args) = do
    closureVal <- interpExpr g env funcExpr
    argVals <- mapM (interpExpr g env) args
    case closureVal of
        ValClosure params retType body capturedEnv -> do
            if length params /= length argVals 
                then error "Incorrect number of arguments for the function."
                else do
                    -- Inference of Generics
                    let pairs = zip params argVals
                    let buildGenericsMap m (Param _ (Just (TyVar tName)), val) = 
                            let vTy = typeOfVal val
                            in case M.lookup tName m of
                                Just expected -> if expected == vTy then m 
                                                 else error $ "Generics conflict: '" ++ tName ++ "' was " ++ show expected ++ ", but received" ++ show vTy
                                Nothing -> M.insert tName vTy m
                        buildGenericsMap m (Param _ (Just (TyArray (TyVar tName) _)), ValArray vTy _) =
                            case M.lookup tName m of
                                Just expected -> if expected == vTy then m 
                                                 else error $ "Generics conflict in the Array: '" ++ tName ++ "'"
                                Nothing -> M.insert tName vTy m
                        buildGenericsMap m _ = m
                        
                    let genericsMap = foldl buildGenericsMap M.empty pairs

                    -- Argument Type Validation
                    mapM_ (\(Param pName pType, val) -> 
                        case pType of
                            Just t -> do
                                let resolvedType = resolveType genericsMap t
                                unless (checkType resolvedType val) $
                                    error $ "Type error in arg '" ++ pName ++ "'. Expected " ++ show resolvedType ++ ", received: " ++ show val
                            Nothing -> return ()
                        ) pairs

                    -- Env config and execution
                    argRefs <- mapM newIORef argVals
                    let argBindings = M.fromList $ zip [pid | Param pid _ <- params] argRefs
                    let execEnv = argBindings `M.union` capturedEnv
                    
                    resultVal <- interpBlock g execEnv body
                    
                    -- Check Return
                    case retType of
                        Just rt -> do
                            let resolvedRetType = resolveType genericsMap rt
                            unless (checkType resolvedRetType resultVal) $
                                error $ "Type error in return. Expected: "++ show resolvedRetType++" Returned: " ++ show resultVal
                            return resultVal
                        Nothing -> return resultVal
        
        _ -> error $ "Attempting to call a value that is not a function: " ++ show closureVal

interpExpr g env (Paren e) = interpExpr g env e


-- Mutability Helpers

-- Auxiliary function to x++ and x-- operations
handleIncDec :: Globals -> Env -> Expr -> Int -> IO Value
handleIncDec g env lvalue delta = do
    -- Get the actual value
    currentVal <- interpExpr g env lvalue
    case currentVal of
        ValInt n -> do
            -- Write the new
            writeValue g env lvalue (ValInt (n + delta))
            -- Return original value (pos-fixed)
            return (ValInt n)
        _ -> error "Increment/Decrement only in Int."

-- Write a value into an Value
-- Var
writeValue :: Globals -> Env -> Expr -> Value -> IO ()
writeValue _ env (Var id) val = do
    case M.lookup id env of
        Just ref -> writeIORef ref val
        Nothing  -> error $ "Var '" ++ id ++ "' not defined."
-- Obj Field
writeValue g env (objExpr :.: field) val = do
    objVal <- interpExpr g env objExpr
    case objVal of
        ValStruct id fields -> case M.lookup field fields of
            Just ref -> writeIORef ref val
            Nothing -> error $ "Field '" ++ field ++ "' not found in '" ++ id ++ "'."
        _ -> error "Invalid field assignment."
-- Array 
writeValue g env (arrExpr :@: idxExpr) val = do
    arrVal <- interpExpr g env arrExpr
    idxVal <- interpExpr g env idxExpr
    case (arrVal, idxVal) of
        (ValArray _ ref, ValInt i) -> do
            list <- readIORef ref
            if i >= 0 && i < length list then do
                let (before, _:after) = splitAt i list
                writeIORef ref (before ++ [val] ++ after)
            else error "Index outside the limits."
        _ -> error "Invalid assignment in array."

writeValue _ _ _ _ = error "Attempting to assign value to an expression that is not Value."

-- ==========================================
-- Block & Statement Interpreter
-- ==========================================

-- Internal flow control
data BlockResult
    = Normal
    | ContinueLoop 
    | ReturnValue Value
    | BreakLoop 

interpBlock :: Globals -> Env -> Block -> IO Value
interpBlock g env (Block stmts) = do
    -- Obtain and execute the statements of the block.
    result <- runStmts env stmts 
    case result of
        ReturnValue v -> return v
        _             -> return ValVoid
  where
    --Executes the statements recursively.
    runStmts :: Env -> [Stmt] -> IO BlockResult
    runStmts _ [] = return Normal
    
    runStmts currentEnv (stmt:rest) = do
        case stmt of
            -- Var declaretion
            VarDecl id maybeType maybeInit -> do
                val <- case maybeInit of
                    Just expr -> interpExpr g currentEnv expr
                    Nothing -> case maybeType of
                        Just t  -> defaultValue t
                        Nothing -> error $ "Variable '" ++ id ++ "' needs an initial type or value in the declaretion."
                
                -- If there is no explicit type, the type is inferred from the value.
                inferredType <- case maybeType of
                        -- Array Case
                        Just (TyArray t Nothing) -> case val of
                            ValArray _ ref -> do
                                list <- readIORef ref
                                return $ TyArray t $ Just $ LitInt $ length list
                            _ -> return $ TyArray t Nothing
                        Just t -> return t
                        Nothing -> case val of
                            ValArray t ref -> do
                                list <- readIORef ref
                                return $ TyArray t (Just (LitInt (length list)))
                            _ -> return (typeOfVal val)
                
                -- TypeCheck
                unless (checkType inferredType val)
                    $ error $ "Type error in variable '"++ id ++ "'. Expected: "++show inferredType++" Received: "++ show val

                -- Create memory ref
                ref <- newIORef val
                let newEnv = M.insert id ref currentEnv
                runStmts newEnv rest

            -- Return
            Return maybeExpr -> do
                val <- case maybeExpr of
                    Just expr -> interpExpr g currentEnv expr
                    Nothing   -> return ValVoid
                return (ReturnValue val)
            
            -- Expr
            Exp expr -> do
                _ <- interpExpr g currentEnv expr
                runStmts currentEnv rest

            -- If / Elif / Else
            IF cond blockIf elifs maybeElse -> do
                condVal <- interpExpr g currentEnv cond
                case condVal of
                    ValBool True -> do
                        res <- interpBlockResult g currentEnv blockIf
                        propagateResult res currentEnv rest
                    ValBool False -> 
                        runElifs currentEnv elifs maybeElse
                    _ -> error "The 'if' condition must be Boolean."

            -- While
            While cond block -> 
                runWhile cond block currentEnv rest

            -- For
            For init cond step block -> do
                _ <- interpExpr g currentEnv init
                runFor cond step block currentEnv rest
            
            -- Continue and Break for loops
            Break -> return BreakLoop
            Continue -> return ContinueLoop

            Print expr -> do
                val <- interpExpr g currentEnv expr
                print val
                runStmts currentEnv rest

            -- Scan
            Scan expr -> do
                currentVal <- interpExpr g currentEnv expr
                inputStr <- getLine
                let newVal = case currentVal of
                        ValInt _    -> ValInt (read inputStr)
                        ValFloat _  -> ValFloat (read inputStr)
                        ValString _ -> ValString inputStr
                        ValBool _   -> case inputStr of
                                         "true"  -> ValBool True
                                         "True"  -> ValBool True
                                         "false" -> ValBool False
                                         "False" -> ValBool False
                                         _       -> error "Invalid input for boolean"
                        _           -> error "This data type is not supported for the scan operation."
                writeValue g currentEnv expr newVal
                runStmts currentEnv rest
            
            where

            -- Flow Control Helpers

                propagateResult (ReturnValue v) _ _ = return (ReturnValue v)
                propagateResult BreakLoop _ _       = return BreakLoop
                propagateResult ContinueLoop _ _    = return ContinueLoop
                propagateResult Normal env rest     = runStmts env rest

                -- Executes a nested block.
                interpBlockResult g' env' (Block s) = runStmts env' s

                -- Elifs
                -- Else 
                runElifs env [] maybeElse = 
                    case maybeElse of
                        Just block -> do
                            res <- interpBlockResult g env block
                            propagateResult res env rest
                        Nothing -> runStmts env rest
                -- Elif
                runElifs env ((cond, block):xs) maybeElse = do
                    val <- interpExpr g env cond
                    case val of
                        ValBool True -> do
                            res <- interpBlockResult g env block
                            propagateResult res env rest
                        ValBool False -> runElifs env xs maybeElse
                        _ -> error "The 'elif' condition must be Boolean."

                -- While Logic
                runWhile cond block env rest = do
                    condVal <- interpExpr g env cond
                    case condVal of
                        ValBool True -> do
                            res <- interpBlockResult g env block
                            case res of
                                ReturnValue v -> return (ReturnValue v)
                                BreakLoop     -> runStmts env rest
                                ContinueLoop  -> runWhile cond block env rest
                                Normal        -> runWhile cond block env rest
                        ValBool False -> runStmts env rest
                        _ -> error "The 'while' condition must be Boolean."
                        
                -- For Logic
                runFor cond step block env rest = do
                    condVal <- interpExpr g env cond
                    case condVal of
                         ValBool True -> do
                             res <- interpBlockResult g env block
                             case res of
                                 ReturnValue v -> return (ReturnValue v)
                                 BreakLoop     -> runStmts env rest
                                 ContinueLoop  -> do
                                     _ <- interpExpr g env step
                                     runFor cond step block env rest
                                 Normal -> do
                                     _ <- interpExpr g env step
                                     runFor cond step block env rest
                         ValBool False -> runStmts env rest
                         _ -> error "The 'for' condition must be Boolean."

-- ==========================================
-- Execution Modes
-- ==========================================

-- Evaluate a list of Decl (SL file)
evalSL :: SL -> Globals -> Globals
evalSL (SL decls) initialGlobals = 
    foldl (flip evalDecl) initialGlobals decls

-- Evaluate onde Decl
evalDecl :: Decl -> Globals -> Globals
evalDecl (Struct id fields) g = 
    let fieldsList = map (\(Field n t) -> (n, t)) fields
    in g { gStructs = M.insert id fieldsList (gStructs g) }
evalDecl f@(Func _ id _ _ _) g = 
    g { gFuncs = M.insert id f (gFuncs g) }


-- File Interpreter
runProgramFile :: SL -> IO ()
runProgramFile ast = do
    let emptyGlobals = Globals M.empty M.empty
    let globals = evalSL ast emptyGlobals
    
    -- Search for main 
    case M.lookup "main" (gFuncs globals) of
        Just (Func _ _ params _ body) -> do
            if null params
                then do
                    _ <- interpBlock globals M.empty body
                    return ()
                else error "The 'main' function should not receive parameters."
        Nothing -> return ()

-- REPL Interpreter
runReplStmt :: Globals -> Env -> Stmt -> IO Env
runReplStmt g env stmt = case stmt of
    -- If it's a variable declaration, create and save it in the Console environment.
    VarDecl id maybeType maybeInit -> do
        val <- case maybeInit of
            Just expr -> interpExpr g env expr
            Nothing -> case maybeType of
                Just t  -> defaultValue t
                Nothing -> error "Console: Variables need a type or initial value."
        
        inferredType <- case maybeType of
            Just (TyArray t Nothing) -> case val of
                ValArray _ ref -> do
                    list <- readIORef ref
                    return $ TyArray t $ Just $ LitInt $ length list
                _ -> return $ TyArray t Nothing
            Just t -> return t
            Nothing -> case val of
                ValArray t ref -> do
                    list <- readIORef ref
                    return $ TyArray t (Just (LitInt (length list)))
                _ -> return (typeOfVal val)

        unless (checkType inferredType val)
            $ error $ "Console Type error: Expected " ++ show inferredType ++ " Received: " ++ show val

        ref <- newIORef val
        return (M.insert id ref env)
        
    -- If it's an isolated expression, evaluate it and print the result
    Exp expr -> do
        val <- interpExpr g env expr
        print val
        return env
        
    -- If it's a control flow, it runs in an isolated scope and returns to the original environment.
    _ -> do
        _ <- interpBlock g env (Block [stmt])
        return env
