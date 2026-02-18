{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Frontend.Parser.Syntax where

import Data.IORef
import qualified Data.Map as M

-- ID to Var, Func, and Struct
type ID = String

data Type 
    -- Base Types --
    = TyInt
    | TyFloat 
    | TyString 
    | TyBool 
    | TyVoid
    -- Composite types -- 
    | TyArray Type (Maybe Expr)
    | TyStruct ID
    | TyFunc [Type] Type
    -- Generic type for inference --
    | TyVar ID
    deriving (Eq, Ord, Show)

-- Main , Struct and Func

data SL = SL [Decl] deriving (Eq, Ord, Show)

data Decl
    = Struct ID [Field]
    | Func Generics ID [Param] (Maybe Type) Block
    deriving (Eq, Ord, Show)

data Field = Field ID Type deriving (Eq, Ord, Show)
data Param = Param ID (Maybe Type) deriving (Eq, Ord, Show)
data Generics = Generics (Maybe [ID])  deriving (Eq, Ord, Show)
data Block = Block [Stmt] deriving (Eq, Ord, Show)

-- Stmt
data Stmt 
    = VarDecl ID (Maybe Type) (Maybe Expr) -- Type/Init are optional for inference
    | Return (Maybe Expr)
    | Print Expr
    | Scan Expr
    | IF Expr Block [(Expr, Block)] (Maybe Block) -- [(Expr, Block)]: Elif, (Maybe Block): Else
    | While Expr Block
    | For Expr Expr Expr Block
    | Exp Expr  -- Isolated expression
    deriving (Eq, Ord, Show)

data Expr
    -- Assignment
    = Expr := Expr

    -- Or and And
    | Expr :||: Expr
    | Expr :&&: Expr

    -- Comparison
    | Expr :==: Expr
    | Expr :!=: Expr
    | Expr :<:  Expr
    | Expr :<=: Expr
    | Expr :>:  Expr
    | Expr :>=: Expr

    -- Arithmetical
    | Expr :+: Expr
    | Expr :-: Expr
    | Expr :*: Expr
    | Expr :/: Expr

    -- Unitary
    | Not Expr      -- !x
    | Neg Expr      -- -x
    
    | PostInc Expr  -- a++
    | PostDec Expr  -- a--

    | Var ID
    | Expr :.: ID  -- Structs Field Access
    | Expr :@: Expr  -- Arrays Position Access

    | FuncCall Expr [Expr]
    | NewObj ID [Expr] 
    | NewArray Type [Expr]
    
    | LitInt Int
    | LitFloat Float
    | LitString String
    | LitBool Bool
    | LitArray [Expr]

    | Paren Expr -- Preserves explicit parentheses
    
    deriving (Eq, Ord, Show)

-- Value definition
data Value
    = ValInt Int
    | ValFloat Float
    | ValString String
    | ValBool Bool
    | ValVoid
    | ValArray (IORef [Value]) 
    | ValStruct ID (M.Map ID (IORef Value))
    | ValClosure [Param] Block (M.Map ID (IORef Value)) 

instance Show Value where
    show (ValInt v)    = show v
    show (ValFloat v)  = show v
    show (ValString v) = v
    show (ValBool v)   = show v
    show ValVoid       = "void"
    show (ValArray _)  = "[Array]"
    show (ValStruct n _) = "<Struct " ++ n ++ ">"
    show (ValClosure {}) = "<Function>"

-- Precedence
infixr 1 :=
infixl 2 :||:
infixl 3 :&&:
infix  4 :==:, :!=:, :<:, :<=:, :>:, :>=:
infixl 6 :+:, :-:
infixl 7 :*:, :/:
infixl 8 :.:, :@:
