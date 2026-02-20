module Frontend.Semantics.Basics where

import qualified Data.Map.Strict as M
import Data.List 

import Frontend.Syntax

--- Contexts

-- Vars
type VarCtx = M.Map ID Type

-- Structs
type StructCtx = M.Map ID [(ID, Type)]

-- Functions
type FuncCtx = M.Map ID ([Type], Type)

-- Returns
type ReturnCtx = Maybe Type

-- Contexto completo
data Ctx = Ctx{
      varCtx     :: VarCtx,
      structCtx  :: StructCtx,
      funcCtx    :: FuncCtx,
      returnCtx  :: ReturnCtx,
      inLoop     :: Bool,
      currentPos :: Pos -- (Int, Int) Follow the current row and column.
    } deriving (Show)

emptyCtx :: Ctx
emptyCtx = Ctx { varCtx = M.empty ,
                 structCtx = M.empty, 
                 funcCtx = M.empty, 
                 returnCtx = Nothing, 
                 inLoop = False,
                 currentPos = (0,0)
                }

--- Semantic errors types 

type Expected = Type
type Found = Type

data ErrorTypes
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

type SemanticError = (ErrorTypes, Pos)
type Check a = Either [SemanticError] a

-- Smart constructors 

typeError :: Ctx -> Expected -> Found -> Check a
typeError ctx e f = Left [(IncompatibleTypes e f, currentPos ctx)]

undefinedVar :: Ctx -> ID -> Check a
undefinedVar ctx id = Left [(UndefinedVariable id, currentPos ctx)]

undefinedFunc :: Ctx -> ID -> Check a
undefinedFunc ctx id = Left [(UndefinedFunction id, currentPos ctx)]

undefinedStruct :: Ctx -> ID -> Check a
undefinedStruct ctx id = Left [(UndefinedStruct id, currentPos ctx)]

undefinedField :: Ctx -> ID -> ID -> Check a
undefinedField ctx s f = Left [(UndefinedField s f, currentPos ctx)]

arityMismatch :: Ctx -> ID -> Int -> Int -> Check a
arityMismatch ctx n e f = Left [(ArityMismatch n e f, currentPos ctx)]

notAnArray :: Ctx -> Type -> Check a
notAnArray ctx t = Left [(NotAnArray t, currentPos ctx)]

notAStruct :: Ctx -> Type -> Check a
notAStruct ctx t = Left [(NotAStruct t, currentPos ctx)]

notCallable :: Ctx -> Type -> Check a
notCallable ctx t = Left [(NotCallable t, currentPos ctx)]

notAssignable :: Ctx -> Expr -> Check a
notAssignable ctx e = Left [(NotAssignable e, currentPos ctx)]

returnMismatch :: Ctx -> Expected -> Found -> Check a
returnMismatch ctx e f = Left [(ReturnTypeMismatch e f, currentPos ctx)]

missingReturn :: Ctx -> ID -> Check a
missingReturn ctx id = Left [(MissingReturn id, currentPos ctx)]

genericError :: Ctx -> String -> Check a
genericError ctx msg = Left [(GenericError msg, currentPos ctx)]

breakOutside :: Ctx -> Check a
breakOutside ctx = Left [(BreakOutsideLoop "Break outside of a loop", currentPos ctx)]

continueOutside :: Ctx -> Check a
continueOutside ctx = Left [(ContinueOutsideLoop "Continue outside of a loop", currentPos ctx)]

duplicateDecl :: Ctx -> ID -> Check a
duplicateDecl ctx id = Left [(DuplicateDecl id, currentPos ctx)]

invalidMain :: Ctx -> String -> Check a
invalidMain ctx msg = Left [(InvalidMainSignature msg, currentPos ctx)]

-- It combines two checks, keeping the error.
bothCheck :: Check a -> Check b -> Check (a, b)
bothCheck (Right a) (Right b) = Right (a, b)
bothCheck (Left e1) (Left e2) = Left (e1 ++ e2)
bothCheck (Left e1) _         = Left e1
bothCheck _         (Left e2) = Left e2

-- Global Helpers

-- Search for a variable in the context
lookupVar :: Ctx -> ID -> Check Type
lookupVar ctx v =
    case M.lookup v (varCtx ctx) of
        Just ty -> Right ty
        Nothing -> undefinedVar ctx v

-- Checks if a type is numeric.
requireNumeric :: Ctx -> Type -> Check Type
requireNumeric _ TyInt   = Right TyInt
requireNumeric _ TyFloat = Right TyFloat
requireNumeric ctx t     = typeError ctx TyInt t

-- Compare Types
requireEqual :: Ctx -> Expected -> Found -> Check Type
requireEqual ctx e f
    | e == f    = Right e
    | otherwise = typeError ctx e f

-- Helper for flexible assignments
checkAssign :: Ctx -> Expected -> Found -> Check Type
checkAssign ctx (TyArray t1 Nothing) (TyArray t2 (Just _)) = do
    _ <- requireEqual ctx t1 t2
    return (TyArray t1 Nothing)
checkAssign ctx (TyArray t1 (Just _)) (TyArray t2 Nothing) = do
    _ <- requireEqual ctx t1 t2
    return (TyArray t1 Nothing)
checkAssign ctx e f = requireEqual ctx e f
