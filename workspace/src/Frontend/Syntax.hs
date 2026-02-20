{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Frontend.Syntax where

import Data.IORef
import qualified Data.Map as M
import Data.List (intercalate)

-- ID to Var, Func, and Struct
type ID = String

-- Help to indentify the position where are some semantic error
type Pos = (Int, Int)
data Loc a = Loc Pos a deriving (Ord, Show)

instance Eq a => Eq (Loc a) where
    (Loc _ a) == (Loc _ b) = a == b

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
    deriving (Eq, Ord)

instance Show Type where
    show TyInt          = "Int"
    show TyFloat        = "Float"
    show TyString       = "String"
    show TyBool         = "Bool"
    show TyVoid         = "Void"
    show (TyArray t Nothing)            = show t ++ "[]"
    show (TyArray t (Just (LitInt n)))  = show t ++ "[" ++ show n ++ "]"
    show (TyArray t (Just _))           = show t ++ "[?]"
    show (TyStruct n)   = "Struct<" ++ n ++ ">"
    show (TyFunc ps r)  = "(" ++ intercalate ", " (map show ps) ++ ") -> " ++ show r
    show (TyVar v)      = "Generic " ++ v

-- Main , Struct and Func

data SL = SL [Loc Decl] deriving (Eq, Ord, Show)

data Decl
    = Struct ID [Loc Field]
    | Func Generics ID [Loc Param] (Maybe Type) Block
    deriving (Eq, Ord, Show)

data Field = Field ID Type deriving (Eq, Ord, Show)
data Param = Param ID (Maybe Type) deriving (Eq, Ord, Show)
data Generics = Generics (Maybe [ID])  deriving (Eq, Ord, Show)
data Block = Block [Loc Stmt] deriving (Eq, Ord, Show)

-- Stmt
data Stmt 
    = VarDecl ID (Maybe Type) (Maybe Expr) -- Type/Init are optional for inference
    | Return (Maybe Expr)
    | Print Expr
    | Scan Expr
    | IF Expr Block [(Expr, Block)] (Maybe Block) -- [(Expr, Block)]: Elif, (Maybe Block): Else
    | Continue
    | Break
    | While Expr Block
    | For Stmt Expr Expr Block
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
defaultValue (TyArray t (Just (LitInt size))) = do
    def <- defaultValue t
    ref <- newIORef (replicate size def)
    return $ ValArray t ref
defaultValue (TyArray t _) = do
    ref <- newIORef []
    return $ ValArray t ref
defaultValue (TyStruct _) = return ValVoid 
defaultValue _            = return ValVoid

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
checkType (TyVar _) _                      = True
checkType _ _                              = False

typeOfVal :: Value -> Type
typeOfVal (ValInt _)       = TyInt
typeOfVal (ValFloat _)     = TyFloat
typeOfVal (ValString _)    = TyString
typeOfVal (ValBool _)      = TyBool
typeOfVal ValVoid          = TyVoid
typeOfVal (ValArray t _)   = TyArray t Nothing
typeOfVal (ValStruct id _) = TyStruct id
typeOfVal (ValClosure params retType _ _) =
    TyFunc (map (\(Param _ t) -> maybe TyVoid id t) params) (maybe TyVoid id retType)

resolveType :: M.Map ID Type -> Type -> Type
resolveType m (TyVar name) = 
    case M.lookup name m of
        Just t -> t
        Nothing -> TyVar name
resolveType m (TyArray t size) = TyArray (resolveType m t) size
resolveType _ t = t
