{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Interpreter where

import Data.IORef
import qualified Data.Map as M
import Control.Monad (zipWithM_, unless)
import Data.Maybe (fromJust)

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
_             .+. _             = error "Unreachable: Type Checker failed"

(.-.) :: Value -> Value -> Value
(ValInt a)   .-. (ValInt b)   = ValInt $ a - b
(ValFloat a) .-. (ValFloat b) = ValFloat $ a - b
(ValFloat a) .-. (ValInt b)   = ValFloat $ a - fromIntegral b
(ValInt a)   .-. (ValFloat b) = ValFloat $ fromIntegral a - b
_            .-. _            = error "Unreachable: Type Checker failed"

(.*.) :: Value -> Value -> Value
(ValInt a)   .*. (ValInt b)   = ValInt $ a * b
(ValFloat a) .*. (ValFloat b) = ValFloat $ a * b
(ValFloat a) .*. (ValInt b)   = ValFloat $ a * fromIntegral b
(ValInt a)   .*. (ValFloat b) = ValFloat $ fromIntegral a * b
_            .*. _            = error "Unreachable: Type Checker failed"

(./.) :: Value -> Value -> Value
(ValInt a)   ./. (ValInt b)   = ValInt $ a `div` b
(ValFloat a) ./. (ValFloat b) = ValFloat $ a / b
(ValFloat a) ./. (ValInt b)   = ValFloat $ a / fromIntegral b
(ValInt a)   ./. (ValFloat b) = ValFloat $ fromIntegral a / b
_            ./. _            = error "Unreachable: Type Checker failed"

-- Comparison
-- not == for !=
(.==.) :: Value -> Value -> Value
(ValInt a)    .==. (ValInt b)    = ValBool $ a == b
(ValFloat a)  .==. (ValFloat b)  = ValBool $ a == b
(ValFloat a)  .==. (ValInt b)    = ValBool $ a == fromIntegral b
(ValInt a)    .==. (ValFloat b)  = ValBool $ fromIntegral a == b
(ValString a) .==. (ValString b) = ValBool $ a == b
_             .==. _             = error "Unreachable: Type Checker failed"

-- not < for >=
(.<.) :: Value -> Value -> Value
(ValInt a)    .<. (ValInt b)    = ValBool $ a < b
(ValFloat a)  .<. (ValFloat b)  = ValBool $ a < b
(ValFloat a)  .<. (ValInt b)    = ValBool $ a < fromIntegral b
(ValInt a)    .<. (ValFloat b)  = ValBool $ fromIntegral a < b
(ValString a) .<. (ValString b) = ValBool $ a < b
_             .<. _             = error "Unreachable: Type Checker failed"

-- not > for <=
(.>.) :: Value -> Value -> Value
(ValInt a)    .>. (ValInt b)    = ValBool $ a > b
(ValFloat a)  .>. (ValFloat b)  = ValBool $ a > b
(ValFloat a)  .>. (ValInt b)    = ValBool $ a > fromIntegral b
(ValInt a)    .>. (ValFloat b)  = ValBool $ fromIntegral a > b
(ValString a) .>. (ValString b) = ValBool $ a > b
_             .>. _             = error "Unreachable: Type Checker failed"

(.||.) :: Value -> Value -> Value
(ValBool a) .||. (ValBool b) = ValBool (a || b)
_           .||. _           = error "Unreachable: Type Checker failed"

(.&&.) :: Value -> Value -> Value
(ValBool a) .&&. (ValBool b) = ValBool (a && b)
_           .&&. _           = error "Unreachable: Type Checker failed"

-- Applies binary options
interpBinOp :: (Value -> Value -> Value) -> Expr -> Expr -> Globals -> Env -> IO Value
interpBinOp f e1 e2 g env = f <$> interpExpr g env e1 <*> interpExpr g env e2

-- Unary
opNot :: Value -> Value
opNot (ValBool b) = ValBool (not b)
opNot _           = error "Unreachable: Type Checker failed"

opNeg :: Value -> Value
opNeg (ValInt n)   = ValInt (-n)
opNeg (ValFloat n) = ValFloat (-n)
opNeg _            = error "Unreachable: Type Checker failed"

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
    return $ ValArray TyVoid ref 

-- Vars
interpExpr g env (Var id) = do
    case M.lookup id env of
        Just ref -> readIORef ref
        Nothing  -> return $ case M.lookup id (gFuncs g) of
                        Just (Func _ _ locParams retType body) -> 
                            let pureParams = map (\(Loc _ p) -> p) locParams
                            in ValClosure pureParams retType body M.empty
                        Nothing -> error "Unreachable: Undefined Var"

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

interpExpr g env (Not e) = opNot <$> interpExpr g env e
interpExpr g env (Neg e) = opNeg <$> interpExpr g env e

interpExpr g env (PostInc value) = handleIncDec g env value 1
interpExpr g env (PostDec value) = handleIncDec g env value (-1)

-- Struct recursive acess
interpExpr g env (objExpr :.: field) = do
    objVal <- interpExpr g env objExpr
    case objVal of
        ValStruct _ fieldsMap -> readIORef (fieldsMap M.! field)
        ValArray _ ref -> do
            list <- readIORef ref
            return $ ValInt $ length list
        _ -> error "Unreachable: Not a struct/array"

-- Array recursive acess
interpExpr g env (arrExpr :@: idxExpr) = do
    ValArray _ ref <- interpExpr g env arrExpr
    ValInt i <- interpExpr g env idxExpr
    list <- readIORef ref
    if i >= 0 && i < length list 
        then return (list !! i)
        else error $ "Runtime Error: Array index out of bounds: " ++ show i

-- Create Obj 
interpExpr g env (NewObj structName args) = do
    argVals <- mapM (interpExpr g env) args
    -- Search for global definition
    let fieldDefs = gStructs g M.! structName 
    
    finalVals <- if null argVals
        then mapM (\(_, t) -> defaultValue t) fieldDefs
        else return argVals
    
    -- Create memory ref
    refs <- mapM newIORef finalVals
    let fieldsMap = M.fromList $ zip (map fst fieldDefs) refs
    return $ ValStruct structName fieldsMap

-- Create Array 
interpExpr g env (NewArray baseType dimsExpr) = do
    dimValues <- mapM (interpExpr g env) dimsExpr
    let sizes = [i | ValInt i <- dimValues] 
    
    -- Check if sizes is valid
    if any (< 0) sizes 
        then error "Runtime Error: Array size cannot be negative."
        else createArrayRecursive baseType sizes
  where
    -- Auxiliary function to create Array with default values
    createArrayRecursive t [size] = do
        def <- defaultValue t 
        vals <- sequence <$> replicate size $ return def 
        ref <- newIORef vals
        return $ ValArray t ref
    createArrayRecursive t (size:rest) = do
        innerArrays <- sequence <$> replicate size $ createArrayRecursive t rest
        ref <- newIORef innerArrays
        return $ ValArray t ref 

-- Function Call
interpExpr g env (FuncCall funcExpr args) = do
    -- Find function declaration 
    ValClosure params _ body capturedEnv <- interpExpr g env funcExpr
    
    argVals <- mapM (interpExpr g env) args
    argRefs <- mapM newIORef argVals
    
    -- Env config and execution
    let argBindings = M.fromList $ zip [pid | Param pid _ <- params] argRefs
    let execEnv = argBindings `M.union` capturedEnv
    
    interpBlock g execEnv body

interpExpr g env (Paren e) = interpExpr g env e


-- Mutability Helpers

-- Auxiliary function to x++ and x-- operations
handleIncDec :: Globals -> Env -> Expr -> Int -> IO Value
handleIncDec g env lvalue delta = do
    -- Get the actual value
    ValInt n <- interpExpr g env lvalue
    -- Write the new
    writeValue g env lvalue (ValInt (n + delta))
    -- Return original value (pos-fixed)
    return (ValInt n)

-- Write a value into an Value
-- Var
writeValue :: Globals -> Env -> Expr -> Value -> IO ()
writeValue _ env (Var id) val = writeIORef (env M.! id) val

-- Obj Field
writeValue g env (objExpr :.: field) val = do
    ValStruct _ fields <- interpExpr g env objExpr
    writeIORef (fields M.! field) val

-- Array 
writeValue g env (arrExpr :@: idxExpr) val = do
    ValArray _ ref <- interpExpr g env arrExpr
    ValInt i <- interpExpr g env idxExpr
    list <- readIORef ref
    if i >= 0 && i < length list then do
        let (before, _:after) = splitAt i list
        writeIORef ref (before ++ [val] ++ after)
    else error "Runtime Error: Index out of bounds."
writeValue _ _ _ _ = error "Unreachable"


-- ==========================================
-- Block & Statement Interpreter
-- ==========================================

-- Internal flow control
data BlockResult = Normal 
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

    -- Executes the statements recursively.
    runStmts :: Env -> [Loc Stmt] -> IO BlockResult
    runStmts _ [] = return Normal
    -- Unpack 'Loc stmt' to ignore positions in execution.
    runStmts currentEnv (Loc _ stmt : rest) = do
        case stmt of
            -- Var declaretion
            VarDecl id maybeType maybeInit -> do
                val <- case maybeInit of
                    Just expr -> interpExpr g currentEnv expr
                    Nothing   -> case maybeType of
                        Just (TyStruct sName) -> interpExpr g currentEnv (NewObj sName [])
                        Just t -> defaultValue t
                        Nothing -> error "Type not defined in the declaration."
                
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
                ValBool b <- interpExpr g currentEnv cond
                if b then do
                    res <- interpBlockResult g currentEnv blockIf
                    propagateResult res currentEnv rest
                else runElifs currentEnv elifs maybeElse
            
            -- While
            While cond block -> runWhile cond block currentEnv rest
            
            -- For
            For initStmt cond step block -> do
                loopEnv <- case initStmt of
                    VarDecl id mTy mInit -> do
                        val <- case mInit of
                            Just expr -> interpExpr g currentEnv expr
                            Nothing   -> defaultValue $ fromJust mTy
                        ref <- newIORef val
                        return (M.insert id ref currentEnv)
                    Exp expr -> do
                        _ <- interpExpr g currentEnv expr
                        return currentEnv
                    _ -> return currentEnv
                
                -- Pass original env and loop env
                runFor cond step block loopEnv currentEnv rest
            
            -- Continue and Break for loop
            Break -> return BreakLoop
            Continue -> return ContinueLoop
            
            -- Print
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
                        ValBool _   -> ValBool (read inputStr)
                        _           -> error "Runtime Error: Scan not supported for this type."
                writeValue g currentEnv expr newVal
                runStmts currentEnv rest
            
            where

                -- Flow Control Helpers
                propagateResult (ReturnValue v) _ _ = return (ReturnValue v)
                propagateResult BreakLoop _ _       = return BreakLoop
                propagateResult ContinueLoop _ _    = return ContinueLoop
                propagateResult Normal env' rest'   = runStmts env' rest'
                
                -- Executes a nested block
                interpBlockResult g' env' (Block s) = runStmts env' s
                
                -- Elifs
                -- Else
                runElifs env [] maybeElse = case maybeElse of
                        Just block -> do
                            res <- interpBlockResult g env block
                            propagateResult res env rest
                        Nothing -> runStmts env rest
                
                -- Elif
                runElifs env ((cond, block):xs) maybeElse = do
                    ValBool b <- interpExpr g env cond
                    if b then do
                        res <- interpBlockResult g env block
                        propagateResult res env rest
                    else runElifs env xs maybeElse
                
                -- While Logic
                runWhile cond block env rest' = do
                    ValBool b <- interpExpr g env cond
                    if b then do
                        res <- interpBlockResult g env block
                        case res of
                            ReturnValue v -> return (ReturnValue v)
                            BreakLoop     -> runStmts env rest'
                            ContinueLoop  -> runWhile cond block env rest'
                            Normal        -> runWhile cond block env rest'
                    else runStmts env rest'
                
                -- env = inner environment of the loop | origEnv = outer environment of the loop
                runFor cond step block env origEnv rest' = do
                    ValBool b <- interpExpr g env cond
                    if b then do
                         res <- interpBlockResult g env block
                         case res of
                             ReturnValue v -> return (ReturnValue v)
                             BreakLoop     -> runStmts origEnv rest' 
                             ContinueLoop  -> do
                                 _ <- interpExpr g env step
                                 runFor cond step block env origEnv rest'
                             Normal -> do
                                 _ <- interpExpr g env step
                                 runFor cond step block env origEnv rest'
                    else runStmts origEnv rest' -- Return to original env 

-- ==========================================
-- Execution Modes
-- ==========================================

-- Evaluate a list of Decl (SL file)
evalSL :: SL -> Globals -> Globals
evalSL (SL decls) initialGlobals = foldl (flip evalDecl) initialGlobals decls

-- Evaluate Decl
evalDecl :: Loc Decl -> Globals -> Globals
evalDecl (Loc _ (Struct id fields)) g = 
    -- Unpacks the `Loc_` from the top-level
    let fieldsList = map (\(Loc _ (Field n t)) -> (n, t)) fields
    in g { gStructs = M.insert id fieldsList (gStructs g) }
evalDecl (Loc _ f@(Func _ id _ _ _)) g =
    -- Saves the function exactly as it is in the AST (with the Localization Parameters intact)
    g { gFuncs = M.insert id f (gFuncs g) }

-- File Interpreter
runProgramFile :: SL -> IO ()
runProgramFile ast = do
    -- Start Globals
    let globals = evalSL ast (Globals M.empty M.empty)
    -- Search for main
    case M.lookup "main" (gFuncs globals) of
        Just (Func _ _ _ _ body) -> do
            _ <- interpBlock globals M.empty body
            return ()
        Nothing -> return ()

-- REPL Interpreter
runReplStmt :: Globals -> Env -> Stmt -> IO Env
runReplStmt g env stmt = case stmt of
    -- If it's a variable declaration, create and save it in the Console environment.
    VarDecl id maybeType maybeInit -> do
        val <- case maybeInit of
            Just expr -> interpExpr g env expr
            Nothing -> defaultValue (fromJust maybeType)
        ref <- newIORef val
        return (M.insert id ref env)
    -- If it's an isolated expression, evaluate it and print the result
    Exp expr -> do
        val <- interpExpr g env expr
        print val
        return env
        
    -- If it's a control flow, it runs in an isolated scope and returns to the original environment.
    _ -> do
        _ <- interpBlock g env (Block [Loc (0,0) stmt])
        return env
