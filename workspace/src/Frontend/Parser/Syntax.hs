module Frontend.Parser.Syntax where

-- ID to Var, Func, and Struct
type ID = String

data Type 
    = TyInt
    | TyFloat 
    | TyString 
    | TyBool 
    | TyVoid 
    | TyID ID 
    | TyArray Type (Maybe Expr) -- Array type with optional size
    | TyFunc [Type] Type  -- a func can be a First-Class Citizen (This might fuck this project in the next stages)
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

    | FuncCall ID [Expr]
    | NewObj ID [Expr] 
    | NewArray Type [Expr]

    | LitInt Int
    | LitFloat Float
    | LitString String
    | LitBool Bool
    | LitArray [Expr] -- Array literal

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