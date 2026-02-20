module Main where

import Test.HUnit
import Lexer (lexerTests)
import Parser (parserTests)
import Semantic (semanticTests)

main :: IO ()
main = do
    putStrLn "=== Running SL Compiler Tests ==="
    
    putStrLn "\n--- Lexer Tests ---"
    _ <- runTestTT lexerTests
    
    putStrLn "\n--- Parser Tests ---"
    _ <- runTestTT parserTests
    
    putStrLn "\n--- Semantic Tests ---"
    _ <- runTestTT semanticTests
    
    return ()
