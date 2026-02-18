module Main where

import Frontend.Lexer.SL (lexer, runAlex)
import Frontend.Lexer.Token
import Frontend.Parser.SL (parseSL)
import Frontend.Parser.Syntax
import Utils.Pretty (printPretty)
import Utils.Tree (printTree)

import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.IO (openFile, hGetContents, hSetEncoding, utf8, stdout, IOMode(ReadMode))

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Pretty FilePath
  deriving (Eq, Show)

main :: IO ()
main = do
    hSetEncoding stdout utf8 -- To print utf8 chars
    args <- getArgs
    let opt = parseOption args
    runOption opt

-- Run the compiler
runOption :: Maybe Option -> IO ()
runOption opt =
    case opt of
        Just (Lexer file)  -> runLexer file
        Just (Parser file) -> runParser printTree file
        Just (Pretty file) -> runParser printPretty file
        Just Help          -> helpMessage
        Nothing            -> do -- If is a invalid arg
            putStrLn "Error: Invalid command or arguments."
            putStrLn "Type 'cabal run sl -- --help' to see available commands."

-- Very the args
parseOption :: [String] -> Maybe Option
parseOption args =
    case args of
        -- 2 args
        (flag : arg : _)
            | flag `elem` ["--lexer", "-l"] -> Just $ Lexer arg
            | flag `elem` ["--parser", "-pt"] -> Just $ Parser arg
            | flag `elem` ["--pretty", "-pp"] -> Just $ Pretty arg
        -- 1 arg
        (flag : _)
            | flag `elem` ["--help", "-h"] -> Just Help
        _ -> Nothing

-- Run Parser and print token (if pass printTree print a tree, if pass printPretty print a SL program code )
runParser :: (SL -> IO ()) -> FilePath ->  IO ()
runParser action file = do
    if takeExtension file /= ".sl" -- Very if file have .sl extension
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do 
        input <- readFileUtf8 file -- Read the chars with UTF-8 code
        case runAlex input parseSL of
            Left err -> putStrLn err -- Print a parser error 
            Right ast -> action ast -- Print the AST

-- Run Lexer and print tokens
runLexer :: FilePath -> IO ()
runLexer file = do
    if takeExtension file /= ".sl" -- Very if file have .sl extension
        then putStrLn $ "Error: File '" ++ file ++ "' must have .sl extension."
    else do
        input <- readFileUtf8 file -- Read the chars with UTF-8 code
        case lexer input of
            Left err -> putStrLn err -- Print a lexer error 
            Right output -> putStr $ unlines $ map mkTokens output -- Print tokens
        where
            -- Format a output string for each kind of Token
            mkTokens (Token (lin, col) lexeme) = "(" ++ desc ++ ", " ++ pos ++ ")"
                where
                    pos = "(" ++ show lin ++ ", " ++ show col ++ ")"
                    desc = case lexeme of
                        -- Base Literals
                        TkIntLit n      -> "INT_LIT " ++ show n
                        TkFloatLit f    -> "FLOAT_LIT " ++ show f
                        TkStringLit s   -> "STRING_LIT \"" ++ s ++ "\""
                        TkBoolLit b     -> "BOOL_LIT " ++ show b
                        TkID s          -> "ID " ++ s

                        -- Symbols
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

                        -- Operation Symbols
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

                        -- Reserved Lexemes
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

                        -- EOF Token 
                        TkEOF           -> "EOF"

-- To read utf8 chars
readFileUtf8 :: FilePath -> IO String
readFileUtf8 file = do
    handle <- openFile file ReadMode
    hSetEncoding handle utf8
    hGetContents handle

-- Print Help Message
helpMessage :: IO ()
helpMessage = putStrLn $ unlines [
    "Usage: cabal sl -- [OPTIONS] [FILE]   Run the compiler",
    "     | cabal sl-tests                 Run the compiler tests",
    "",
    "Options:",
    "  -l,  --lexer  <file>    Run the lexical analyzer on the given file.",
    "  -pt, --parser <file>    Run the lexical and parser analyzer on the given file and print the Abstract Syntax Tree (AST).",
    "  -pp, --pretty <file>    Run the lexical and parser analyzer on the given file and pretty-print the source code.",
    "  -h,  --help            Show this help message.",
    "",
    "Examples:",
    "  cabal run sl -- --lexer input.sl",
    "  cabal run sl -- --parser  input.sl",
    "  cabal run sl -- --pretty input.sl"
    ]
