module Lexer where

import Test.HUnit
import Frontend.Lexer.SL (runAlex, alexMonadScan)
import Frontend.Lexer.Token
import Frontend.Lexer.Token (Lexeme(TkOr))

scanMany :: String -> Either String [Token]
scanMany input = runAlex input gather
  where
    gather = do
      t <- alexMonadScan
      case t of
        Token _ TkEOF -> return []
        token -> do
          rest <- gather
          return (token : rest)

testKeywords :: Test
testKeywords = TestCase $ do
    let input = "struct forall func let return print if elif else while for new int float string bool void"
    let expected = [TkStruct, TkForAll, TkFunc, TkLet, TkReturn, TkPrint, TkIF, TkElif, TkElse, TkWhile, TkFor, TkNew, TkInt, TkFloat , TkString  , TkBool, TkVoid]
    case scanMany input of
        Right tokens -> assertEqual "All keywords must be recognized: " expected $ map tokenType tokens
        Left e -> assertFailure e

testLiterals :: Test
testLiterals = TestCase $ do
    let input = "123 45.670 \"string\" true ID"
    let res = scanMany input
    case res of
        Right [Token _ (TkIntLit 123), Token _ (TkFloatLit 45.67), Token _ (TkStringLit "string"), Token _ (TkBoolLit True), Token _ (TkID {out = "ID"})] -> return ()
        Right x -> assertFailure $ "Incorrect Literals: " ++ show x
        Left e -> assertFailure e

testOperators :: Test
testOperators = TestCase $ do
    let input = "+ - * / ++ -- = == != < <= > >= && || !"
    let expected = [TkAdd, TkSub, TkMul, TkDiv, TkInc , TkDec , TkAsg, TkEq  , TkNotEq, TkLss, TkLssEq, TkGtr, TkGtrEq, TkAnd , TkOr, TkNot]
    case scanMany input of
        Right tokens -> assertEqual "Mixed operators: " expected $ map tokenType tokens
        Left e -> assertFailure e

testDeliPunct :: Test
testDeliPunct = TestCase $ do
    let input = "( ) [ ] { } () [] {} . , : ; ->"
    let expected = [TkLParen, TkRParen, TkLBracket, TkRBracket, TkLBrace, TkRBrace, TkLParen, TkRParen, TkLBracket, TkRBracket, TkLBrace, TkRBrace, TkDot, TkComma, TkColon, TkSemiColon, TkArrow]
    case scanMany input of
        Right tokens -> assertEqual "All delimiters and punctuation must be recognized: " expected $ map tokenType tokens
        Left e -> assertFailure e

lexerTests :: Test
lexerTests = TestList [
      TestLabel "Keywords" testKeywords
    , TestLabel "Literals" testLiterals
    , TestLabel "Operators" testOperators
    , TestLabel "Delimiters & Punctuation" testDeliPunct
    ]

-- Token Helper
tokenType :: Token -> Lexeme
tokenType (Token _ c) = c