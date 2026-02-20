module Main where

import Frontend.Lexer.SL (lexer, runAlex)
import Frontend.Lexer.Token
import Frontend.Parser.SL (parseSL)
import Frontend.Syntax
import Frontend.Semantics.StmtTC (tychProgram, tychStmt, registerDecl, tychDecl)
import Frontend.Semantics.Basics (ErrorTypes, emptyCtx, Ctx)
import Interpreter (runProgramFile, runReplStmt, evalDecl, Globals(..), Env)
import Utils.Pretty (printPretty)
import Utils.Tree (printTree)

import System.Console.Haskeline -- Importado para suportar setas e histórico
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO (openFile, hGetContents, hSetEncoding, utf8, stdout, stdin, hFlush, IOMode(ReadMode))
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Control.Exception (catch, ErrorCall(..))

data Option
  = Help
  | Repl
  | Lexer FilePath
  | Parser FilePath
  | Pretty FilePath
  | TypeCheck FilePath
  | Interpreter FilePath
  deriving (Eq, Show)

main :: IO ()
main = do
    hSetEncoding stdout utf8 -- To print utf8 chars
    hSetEncoding stdin utf8  -- To input utf8 chars
    args <- getArgs
    let opt = parseOption args
    runOption opt

-- Run the compiler
runOption :: Maybe Option -> IO ()
runOption opt =
    case opt of
        Just (Lexer file)       -> runLexer file
        Just (Parser file)      -> runParser printTree file
        Just (Pretty file)      -> runParser printPretty file
        Just (TypeCheck file)   -> runTypeCheck file
        Just (Interpreter file) -> runInterpreter file
        Just Repl               -> runRepl
        Just Help               -> helpMessage
        Nothing                 -> do -- If is a invalid arg
            putStrLn "Error: Invalid command or arguments."
            putStrLn "Type 'cabal run sl -- --help' to see available commands."

-- Verify the args
parseOption :: [String] -> Maybe Option
parseOption args =
    case args of
        -- 2 args
        [flag, arg]
            | flag `elem` ["--lexer", "-l"]       -> Just $ Lexer arg
            | flag `elem` ["--parser", "-pt"]     -> Just $ Parser arg
            | flag `elem` ["--pretty", "-pp"]     -> Just $ Pretty arg
            | flag `elem` ["--typecheck", "-tc"]  -> Just $ TypeCheck arg
            | flag `elem` ["--interpreter", "-i"] -> Just $ Interpreter arg
        -- 1 arg
        [flag]
            | flag `elem` ["--help", "-h"] -> Just Help
            | flag `elem` ["--repl", "-r"] -> Just Repl
        -- 0 args (Default to REPL)
        [] -> Just Repl
        _ -> Nothing

-- ==========================================
-- Compiler Modes 
-- ==========================================

-- Run Lexer, Parser, Type Checker and Interpreter!
runInterpreter :: FilePath -> IO ()
runInterpreter file = do
    if takeExtension file /= ".sl"
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do 
        input <- readFileUtf8 file
        case runAlex input parseSL of
            Left err -> putStrLn $ "Parser Error: " ++ err
            Right ast -> 
                case tychProgram ast of
                    Left errs -> do
                        putStrLn "Semantic Analysis Failed with the following errors:"
                        mapM_ printSemanticError errs
                    Right () -> runProgramFile ast

-- Run Lexer, Parser and Type Checker
runTypeCheck :: FilePath -> IO ()
runTypeCheck file = do
    if takeExtension file /= ".sl"
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do 
        input <- readFileUtf8 file
        case runAlex input parseSL of
            Left err -> putStrLn $ "Parser Error: " ++ err
            Right ast -> 
                case tychProgram ast of
                    Right () -> putStrLn "Semantic Analysis: Success! No type errors found."
                    Left errs -> do
                        putStrLn "Semantic Analysis Failed with the following errors:"
                        mapM_ printSemanticError errs

-- Formats the Type Checker error messages with row and column
printSemanticError :: (ErrorTypes, Pos) -> IO ()
printSemanticError (errType, (row, col)) =
    putStrLn $ "  [Line " ++ show row ++ ", Col " ++ show col ++ "] " ++ show errType

-- Run Parser and print the AST
runParser :: (SL -> IO ()) -> FilePath ->  IO ()
runParser action file = do
    if takeExtension file /= ".sl" 
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do 
        input <- readFileUtf8 file 
        case runAlex input parseSL of
            Left err -> putStrLn err 
            Right ast -> action ast 

-- Run Lexer and print tokens
runLexer :: FilePath -> IO ()
runLexer file = do
    if takeExtension file /= ".sl" 
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do
        input <- readFileUtf8 file 
        case lexer input of
            Left err -> putStrLn err 
            Right output -> putStr $ unlines $ map mkTokens output 
        where
            mkTokens (Token (lin, col) lexeme) = "(" ++ desc ++ ", " ++ pos ++ ")"
                where
                    pos = "(" ++ show lin ++ ", " ++ show col ++ ")"
                    desc = case lexeme of
                        TkIntLit n      -> "INT_LIT " ++ show n
                        TkFloatLit f    -> "FLOAT_LIT " ++ show f
                        TkStringLit s   -> "STRING_LIT \"" ++ s ++ "\""
                        TkBoolLit b     -> "BOOL_LIT " ++ show b
                        TkID s          -> "ID " ++ s
                        TkLParen        -> "L_PAREN"
                        TkRParen        -> "R_PAREN"
                        TkLBracket      -> "L_BRACKET"
                        TkRBracket      -> "R_BRACKET"
                        TkLBrace        -> "L_BRACE"
                        TkRBrace        -> "R_BRACE"
                        TkDot           -> "DOT"
                        TkComma         -> "COMMA"
                        TkColon         -> "COLON"
                        TkSemiColon     -> "SEMICOLON"
                        TkArrow         -> "->"
                        TkAdd           -> "+"
                        TkSub           -> "-"
                        TkMul           -> "*"
                        TkDiv           -> "/"
                        TkInc           -> "++"
                        TkDec           -> "--"
                        TkAsg           -> "="
                        TkEq            -> "=="
                        TkNotEq         -> "!="
                        TkLss           -> "<"
                        TkLssEq         -> "<="
                        TkGtr           -> ">"
                        TkGtrEq         -> ">="
                        TkAnd           -> "&&"
                        TkOr            -> "||"
                        TkNot           -> "!"
                        TkContinue      -> "continue"
                        TkBreak         -> "break"
                        TkStruct        -> "struct"
                        TkForAll        -> "forall"
                        TkFunc          -> "func"
                        TkLet           -> "let"
                        TkReturn        -> "return"
                        TkPrint         -> "print"
                        TkScan          -> "scan"
                        TkIF            -> "if"
                        TkElif          -> "elif"
                        TkElse          -> "else"
                        TkWhile         -> "while"
                        TkFor           -> "for"
                        TkNew           -> "new"
                        TkInt           -> "int"
                        TkFloat         -> "float"
                        TkString        -> "string"
                        TkBool          -> "bool"
                        TkVoid          -> "void"
                        TkEOF           -> "EOF"

-- ==========================================
-- REPL  
-- ==========================================

-- Starts REPL
runRepl :: IO ()
runRepl = do
    putStrLn "================================================="
    putStrLn " Welcome to REPL SL!"
    putStrLn " Type ':q' or ':quit' to exit."
    putStrLn "================================================="
    let initialGlobals = Globals M.empty M.empty
    -- Roda o REPL dentro do monad Haskeline
    runInputT defaultSettings $ replLoop emptyCtx initialGlobals M.empty

-- Execution Loop 
replLoop :: Ctx -> Globals -> Env -> InputT IO ()
replLoop ctx globals env = do
    minput <- getInputLine "SL> "
    case minput of
        Nothing -> return () -- Ctrl+D ou erro finaliza silenciosamente
        Just input -> do
            let trimmedInput = dropWhile (== ' ') input

            if trimmedInput == ":q" || trimmedInput == ":quit"
                then return ()
                else if null trimmedInput
                then replLoop ctx globals env
                else do
                    let isDecl = any (`isPrefixOf` trimmedInput) ["func ", "struct ", "forall "]
                    if isDecl
                        then do
                            -- liftIO eleva a execução IO para o InputT IO
                            (newCtx, newGlobals, newEnv) <- liftIO $ handleDecl input ctx globals env
                            replLoop newCtx newGlobals newEnv
                        else do
                            (newCtx, newGlobals, newEnv) <- liftIO $ handleStmt trimmedInput ctx globals env
                            replLoop newCtx newGlobals newEnv

-- Statement Routing (Structs/Funcs)
handleDecl :: String -> Ctx -> Globals -> Env -> IO (Ctx, Globals, Env)
handleDecl input ctx globals env = do
    -- Run Lexer and Parser
    case runAlex input parseSL of
        Left err -> do
            putStrLn $ "Syntax Error in Declaration: " ++ err
            return (ctx, globals, env)
        Right (SL decls) -> do
            -- Run TypeChecker 
            case addDeclsToCtx ctx decls of
                Left errs -> do
                    putStrLn "Semantic Error: "
                    mapM_ printSemanticError errs
                    return (ctx, globals, env)
                -- Evaluate and Register
                Right newCtx -> do
                    let newGlobals = foldl (flip evalDecl) globals decls
                    putStrLn "-> Declaration successfully registered."
                    return (newCtx, newGlobals, env)

-- Semantic Analysis of a Decl
addDeclsToCtx :: Ctx -> [Loc Decl] -> Either [(ErrorTypes, Pos)] Ctx
addDeclsToCtx initialCtx decls = do
    ctxWithSigs <- foldM registerDecl initialCtx decls
    mapM_ (tychDecl ctxWithSigs) decls
    return ctxWithSigs

-- Command Routing (Statements)
handleStmt :: String -> Ctx -> Globals -> Env -> IO (Ctx, Globals, Env)
handleStmt input ctx globals env = do
    -- Run isolated lexer and Parser
    case parseReplInput input of
        Left err -> do
            putStrLn $ "Syntax Error: " ++ err
            return (ctx, globals, env)
        Right stmts -> do
            -- Run isolated TypeChecker
            case checkReplStmts ctx stmts of
                Left errs -> do
                    putStrLn "Semantic Error: "
                    mapM_ printSemanticError errs
                    return (ctx, globals, env)
                -- Evaluate and Exec
                Right newCtx -> do
                    -- Catch any Haskell errors.
                    newEnv <- catch (execReplStmts globals env stmts) 
                                    (\(ErrorCall msg) -> do
                                        putStrLn msg -- Just print error
                                        return env)  -- Continue at the evn without change
                    return (newCtx, globals, newEnv)

-- Verify comand structure 
parseReplInput :: String -> Either String [Loc Stmt]
parseReplInput code = 
    let fullCode1 = "func main() { " ++ code ++ " }"
        fullCode2 = "func main() { " ++ code ++ "; }"
    in case runAlex fullCode1 parseSL of
        -- Tenta analisar o código exatamente como o utilizador digitou
        Right (SL [Loc _ (Func _ _ _ _ (Block stmts))]) -> Right stmts
        Left err1 ->
            -- Se falhar, tenta novamente adicionando um ";"
            case runAlex fullCode2 parseSL of
                Right (SL [Loc _ (Func _ _ _ _ (Block stmts))]) -> Right stmts
                -- Se continuar a falhar, devolve o erro original
                Left _ -> Left err1

-- Semantic Analysis of a Comand
checkReplStmts :: Ctx -> [Loc Stmt] -> Either [(ErrorTypes, Pos)] Ctx
checkReplStmts = foldM tychStmt

-- Execute
execReplStmts :: Globals -> Env -> [Loc Stmt] -> IO Env
execReplStmts g = foldM (\e (Loc _ s) -> runReplStmt g e s)

-- ==========================================
-- Utility Functions
-- ==========================================

readFileUtf8 :: FilePath -> IO String
readFileUtf8 file = do
    handle <- openFile file ReadMode
    hSetEncoding handle utf8
    hGetContents handle

helpMessage :: IO ()
helpMessage = putStrLn $ unlines [
    "Usage: cabal run sl -- [OPTIONS] [FILE]   Run the compiler",
    "     | cabal run sl-tests                 Run the compiler tests",
    "",
    "Options:",
    "  -l,  --lexer  <file>       Run the lexical analyzer on the given file.",
    "  -pt, --parser <file>       Run the lexical and parser analyzer to print the AST.",
    "  -pp, --pretty <file>       Run the lexical and parser analyzer to pretty-print code.",
    "  -tc, --typecheck <file>    Run the Semantic Analyzer to check for type errors.",
    "  -i,  --interpreter <file>  Run the interpreter to execute an SL program.",
    "  -h,  --help                Show this help message.",
    "  -r,  --repl                Starts the REPL interpreter console for the SL language.",  
    "",
    "Examples:",
    "  cabal run sl -- --interpreter input.sl",
    "  cabal run sl -- --repl"
    ]
