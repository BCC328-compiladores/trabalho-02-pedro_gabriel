{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Frontend.Lexer.SL where

import Frontend.Lexer.Token
import Control.Monad
}

%wrapper "monadUserState"

$digit      = 0-9
$alpha      = [a-zA-Z]

@id         = $alpha [$alpha $digit \_]*
@int        = $digit+
@float      = $digit+ \. $digit+
@string     = \" [^\"]* \"

tokens:-
      -- Whitespace & Line Comments
      <0> $white+                    ;
      <0> "//".*                     ;

      -- Multi-Line Comment
      <0> "\*"                       { nestComment `andBegin` state_comment }
      <0> "*/"                       {\ _ _ -> alexError "Error! Unexpected close comment!" }
      <state_comment> "\*"           { nestComment }
      <state_comment> "*/"           { unnestComment }
      <state_comment> .              ;
      <state_comment> \n             ;

      -- Keywords
      <0> "struct"                   { simpleToken TkStruct }
      <0> "forall"                   { simpleToken TkForAll }
      <0> "func"                     { simpleToken TkFunc }
      <0> "let"                      { simpleToken TkLet }
      <0> "return"                   { simpleToken TkReturn }
      <0> "print"                    { simpleToken TkPrint }
      <0> "scan"                     { simpleToken TkScan }
      <0> "if"                       { simpleToken TkIF }
      <0> "elif"                     { simpleToken TkElif }
      <0> "else"                     { simpleToken TkElse }
      <0> "while"                    { simpleToken TkWhile }
      <0> "for"                      { simpleToken TkFor }
      <0> "new"                      { simpleToken TkNew }
      <0> "int"                      { simpleToken TkInt }
      <0> "float"                    { simpleToken TkFloat }
      <0> "string"                   { simpleToken TkString }
      <0> "bool"                     { simpleToken TkBool }
      <0> "void"                     { simpleToken TkVoid }

      -- Literals
      <0> @string                    { mkString }
      <0> @float                     { mkFloat }
      <0> @int                       { mkInt }
      <0> "true"                     { mkBool True }
      <0> "false"                    { mkBool False }

      <0> @id                        { mkId }

      -- Delimiters & Punctuation
      <0> "->"                       { simpleToken TkArrow }
      <0> "("                        { simpleToken TkLParen }
      <0> ")"                        { simpleToken TkRParen }
      <0> "["                        { simpleToken TkLBracket }
      <0> "]"                        { simpleToken TkRBracket }
      <0> "{"                        { simpleToken TkLBrace }
      <0> "}"                        { simpleToken TkRBrace }
      <0> "."                        { simpleToken TkDot }
      <0> ","                        { simpleToken TkComma }
      <0> ":"                        { simpleToken TkColon }
      <0> ";"                        { simpleToken TkSemiColon }

      -- Operators
      <0> "++"                       { simpleToken TkInc }
      <0> "--"                       { simpleToken TkDec }
      <0> "=="                       { simpleToken TkEq }
      <0> "!="                       { simpleToken TkNotEq }
      <0> "<="                       { simpleToken TkLssEq }
      <0> ">="                       { simpleToken TkGtrEq }
      <0> "&&"                       { simpleToken TkAnd }
      <0> "||"                       { simpleToken TkOr }
      <0> "+"                        { simpleToken TkAdd }
      <0> "-"                        { simpleToken TkSub }
      <0> "*"                        { simpleToken TkMul }
      <0> "/"                        { simpleToken TkDiv }
      <0> "!"                        { simpleToken TkNot }
      <0> "="                        { simpleToken TkAsg }
      <0> "<"                        { simpleToken TkLss }
      <0> ">"                        { simpleToken TkGtr }
{


-- User state
data AlexUserState = AlexUserState { nestLevel :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s { alex_ust = s' }, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s { alex_ust = f (alex_ust s) }, ())

-- Multi-line comments aux
nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s { nestLevel = nestLevel s + 1 }
  skip input len

unnestComment :: AlexAction Token
unnestComment input len = do
  s <- get
  let level = (nestLevel s) - 1
  put s { nestLevel = level }
  when (level <= 0) $ alexSetStartCode 0
  skip input len

-- Definition of the EOF token
alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment" -- If finish without close a comment
  pure $ Token (position pos) TkEOF

-- Position converter
position :: AlexPosn -> (Int, Int)
position (AlexPn _ line col) = (line, col)

-- Token creation 
simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (pos, _, _, _) _ = return $ Token (position pos) lx

mkInt :: AlexAction Token
mkInt (pos, _, _, str) len =
  pure $ Token (position pos) $ TkIntLit $ read $ take len str

mkFloat :: AlexAction Token
mkFloat (pos, _, _, str) len =
  pure $ Token (position pos) $ TkFloatLit $ read $ take len str

mkString :: AlexAction Token
mkString (pos, _, _, str) len =
  let 
    raw = take len str
    content = init (tail raw) -- Remove " ... "
  in 
    pure $ Token (position pos) $ TkStringLit content

mkBool :: Bool -> AlexAction Token
mkBool b (pos, _, _, _) _ = pure $ Token (position pos) (TkBoolLit b)

mkId :: AlexAction Token
mkId (pos, _, _, str) len =
  pure $ Token (position pos) $ TkID $ take len str

-- Main Function

lexer :: String -> Either String [Token]
lexer input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TkEOF
        then pure [output]
        else (output :) <$> go

}
