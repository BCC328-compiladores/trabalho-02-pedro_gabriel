{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Frontend.Syntax where

import Data.IORef
import qualified Data.Map as M

-- ID to Var, Func, and Struct
type ID = String

-- ==========================================
-- AST
-- ==========================================

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

-- Precedence
infixr 1 :=
infixl 2 :||:
infixl 3 :&&:
infix  4 :==:, :!=:, :<:, :<=:, :>:, :>=:
infixl 6 :+:, :-:
infixl 7 :*:, :/:
infixl 8 :.:, :@:

-- ==========================================
-- Type System
-- ==========================================

data Value
    = ValInt Int
    | ValFloat Float
    | ValString String
    | ValBool Bool
    | ValVoid
    | ValArray Type (IORef [Value]) 
    | ValStruct ID (M.Map ID (IORef Value))
    | ValClosure [Param] (Maybe Type) Block (M.Map ID (IORef Value)) 

instance Show Value where
    show (ValInt v)      = show v
    show (ValFloat v)    = show v
    show (ValString v)   = v
    show (ValBool v)     = show v
    show ValVoid         = "void"
    show (ValArray ty _) = "[Array " ++ show ty ++ "]"
    show (ValStruct n _) = "<Struct " ++ n ++ ">"
    show (ValClosure params retT _ _) = 
        "<Func: (" ++ showParams params ++ ") -> " ++ showRet retT ++ ">"
      where
        showRet Nothing = "void"
        showRet (Just t) = show t
        showParams [] = ""
        showParams ps = unwords [show t | Param _ (Just t) <- ps]

defaultValue :: Type -> IO Value
defaultValue TyInt        = return $ ValInt 0
defaultValue TyFloat      = return $ ValFloat 0.0
defaultValue TyString     = return $ ValString ""
defaultValue TyBool       = return $ ValBool False
defaultValue TyVoid       = return ValVoid
defaultValue (TyArray t _) = do
    ref <- newIORef []
    return $ ValArray t ref
defaultValue (TyStruct _) = return ValVoid 
defaultValue _            = return ValVoid

checkValue :: Value -> Value -> Bool
checkValue (ValInt _)        (ValInt _)        = True
checkValue (ValFloat _)      (ValFloat _)      = True
checkValue (ValString _)     (ValString _)     = True
checkValue (ValBool _)       (ValBool _)       = True
checkValue ValVoid           ValVoid           = True
checkValue (ValArray t1 _)   (ValArray t2 _)   = t1 == t2
checkValue (ValStruct id1 _) (ValStruct id2 _) = id1 == id2
checkValue (ValClosure p1 r1 _ _) (ValClosure p2 r2 _ _) = 
    r1 == r2 && compareParams p1 p2
  where
    compareParams [] [] = True
    compareParams (Param _ tA : xs) (Param _ tB : ys) = tA == tB && compareParams xs ys
    compareParams _ _ = False
checkValue _ _ = False

checkType :: Type -> Value -> Bool
checkType TyInt (ValInt _)                 = True
checkType TyFloat (ValFloat _)             = True
checkType TyString (ValString _)           = True
checkType TyBool (ValBool _)               = True
checkType TyVoid ValVoid                   = True
checkType (TyStruct id1) (ValStruct id2 _) = id1 == id2
checkType (TyStruct _) ValVoid             = True 
checkType (TyArray t1 _) (ValArray t2 _)   = t1 == t2
checkType (TyArray _ _) ValVoid            = True
checkType _ _                              = False
