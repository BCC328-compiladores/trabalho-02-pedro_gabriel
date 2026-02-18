module Frontend.Lexer.Token where

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
    -- Types & ID Lexemes
    = TkIntLit Int
    | TkFloatLit Float
    | TkStringLit String  
    | TkBoolLit Bool
    | TkID { out :: String }
    -- Delimiters & Punctuation Lexemes
    | TkLParen                  -- "("
    | TkRParen                  -- ")"
    | TkLBracket                -- "["
    | TkRBracket                -- "]"
    | TkLBrace                  -- "{"
    | TkRBrace                  -- "}"
    | TkDot                     -- "."
    | TkComma                   -- ","
    | TkColon                   -- ":"
    | TkSemiColon               -- ";"
    | TkArrow                   -- "->"
    -- Operators Lexemes
    | TkAdd                     -- "+"
    | TkSub                     -- "-"
    | TkMul                     -- "*"
    | TkDiv                     -- "/"
    | TkInc                     -- "++"
    | TkDec                     -- "--"
    | TkAsg                     -- "="
    | TkEq                      -- "=="
    | TkNotEq                   -- "!="
    | TkLss                     -- "<"
    | TkLssEq                   -- "<="
    | TkGtr                     -- ">"
    | TkGtrEq                   -- ">="
    | TkAnd                     -- "&&"
    | TkOr                      -- "||"
    | TkNot                     -- "!"
    -- Keywords Lexemes
    | TkContinue
    | TkBreak
    | TkStruct
    | TkForAll
    | TkFunc
    | TkLet
    | TkReturn
    | TkPrint
    | TkScan
    | TkIF
    | TkElif
    | TkElse
    | TkWhile
    | TkFor
    | TkNew
    | TkInt
    | TkFloat 
    | TkString  
    | TkBool
    | TkVoid
    | TkEOF
    deriving (Eq, Ord, Show)
