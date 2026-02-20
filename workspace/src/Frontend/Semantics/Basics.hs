module Frontend.Semantics.Basics where

import qualified Data.Map.Strict as M

import Frontend.Syntax

--- Contexts

-- Vars
type VarCtx = M.Map ID Type

-- Structs
type StructCtx = M.Map ID (M.Map ID Type)

-- Functions
type FuncCtx = M.Map ID ([Type], Type)

-- Returns
type ReturnCtx = Maybe Type

-- Contexto completo
data Ctx = Ctx{
      varCtx    :: VarCtx,
      structCtx :: StructCtx,
      funcCtx   :: FuncCtx,
      returnCtx :: ReturnCtx,
      inLoop    :: Bool
    } deriving (Show)

emptyCtx :: Ctx
emptyCtx = Ctx { varCtx = M.empty , structCtx = M.empty, funcCtx = M.empty, returnCtx = Nothing, inLoop = False }

--- Semantic errors types 

type Expected = Type
type Found = Type

-- Erros semânticos
data SemanticError
    = IncompatibleTypes    Expected Found
    | UndefinedVariable    ID
    | UndefinedFunction    ID
    | UndefinedStruct      ID
    | UndefinedField       ID ID
    | ArityMismatch        ID Int Int
    | NotAnArray           Type
    | NotAStruct           Type
    | NotCallable          Type
    | NotAssignable        Expr
    | ReturnTypeMismatch   Expected Found
    | MissingReturn        ID
    | DuplicateDecl        ID
    | GenericError         String
    | ContinueOutsideLoop  String
    | BreakOutsideLoop     String
    | InvalidMainSignature String 
    deriving (Show)


type Check a = Either [SemanticError] a

-- Smart constructors 

typeError :: Expected -> Found -> Check a
typeError e f = Left [IncompatibleTypes e f]

undefinedVar :: ID -> Check a
undefinedVar = Left . (:[]) . UndefinedVariable

undefinedFunc :: ID -> Check a
undefinedFunc = Left . (:[]) . UndefinedFunction

undefinedStruct :: ID -> Check a
undefinedStruct = Left . (:[]) . UndefinedStruct

undefinedField :: ID -> ID -> Check a
undefinedField s f = Left [UndefinedField s f]

arityMismatch :: ID -> Int -> Int -> Check a
arityMismatch n e f = Left [ArityMismatch n e f]

notAnArray :: Type -> Check a
notAnArray = Left . (:[]) . NotAnArray

notAStruct :: Type -> Check a
notAStruct = Left . (:[]) . NotAStruct

notCallable :: Type -> Check a
notCallable = Left . (:[]) . NotCallable

returnMismatch :: Expected -> Found -> Check a
returnMismatch e f = Left [ReturnTypeMismatch e f]

genericError :: String -> Check a
genericError = Left . (:[]) . GenericError

-- Combina duas checagens, mantendo o erro
bothCheck :: Check a -> Check b -> Check (a, b)
bothCheck (Right a) (Right b) = Right (a, b)
bothCheck (Left e1) (Left e2) = Left (e1 ++ e2)
bothCheck (Left e1) _         = Left e1
bothCheck _         (Left e2) = Left e2

-- Convertendo tipo para string (para print)
strfyType :: Type -> String
strfyType TyInt          = "Int"
strfyType TyFloat        = "Float"
strfyType TyString       = "String"
strfyType TyBool         = "Bool"
strfyType TyVoid         = "Void"
strfyType (TyArray t _)  = "[" ++ strfyType t ++ "]"
strfyType (TyStruct n)   = "Struct<" ++ n ++ ">"
strfyType (TyFunc ps r)  = "(" ++ unwords (map strfyType ps) ++ ") -> " ++ strfyType r
strfyType (TyVar v)      = "?" ++ v 
