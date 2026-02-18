module Utils.Tree where

import Frontend.Syntax
import Data.Tree (Tree(..), drawTree)


class ToTree a where
    toTree :: a -> Tree String

printTree :: SL -> IO ()
printTree sl = putStrLn $ drawTree $ toTree sl

-- Main & Type

instance ToTree SL where
    toTree (SL decls) = Node "SL (Program)" $ map toTree decls

instance ToTree Type where
    toTree t = Node ("Type: " ++ show t) []

-- Decl (Struct / Func)

instance ToTree Decl where
    toTree (Struct id fields) =
        Node ("Struct: " ++ id) $ map toTree fields
    toTree (Func generics id params retType block) =
        Node ("Func: " ++ id)
             [ toTree generics
             , Node "Params" $ map toTree params
             , Node ("ReturnType: " ++ show retType) []
             , toTree block
             ]

instance ToTree Field where
    toTree (Field id type_) = Node ("New Field: " ++ id) [toTree type_]

instance ToTree Generics where
    toTree (Generics Nothing) = Node "Generics: None" [] -- If is a non generic function
    toTree (Generics (Just ids)) = Node ("Generics: " ++ show ids) []

instance ToTree Param where
    toTree (Param id type_) = Node ("Param: " ++ id) $ maybe [] (\t -> [toTree t]) type_

-- Stmt

instance ToTree Block where
    toTree (Block stmts) = Node "Block" $ map toTree stmts

instance ToTree Stmt where
    toTree (VarDecl id type_ init) =
        Node ("New Var: " ++ id) $ 
            -- Handle type inference
            maybe [Node "Type: Inferred" []] (\t -> [toTree t]) type_ ++ 
            maybe [] (\e -> [toTree e]) init

    toTree (Return expr) = Node "Return" $ maybe [] (\e -> [toTree e]) expr

    toTree (Print expr) = Node "Print" [toTree expr]

    toTree (Scan expr) = Node "Scan" [toTree expr]

    toTree Continue = Node "Continue" []
     
    toTree Break = Node "Break" []

    toTree (IF cond block elifs elseBlock) =
        Node "If" $ [toTree cond, toTree block] ++                    -- If
            map elifToTree elifs ++                                   -- Elif 
            maybe [] (\b -> [Node "Else" [toTree b]]) elseBlock       -- Else
      where
        elifToTree (e, b) = Node "Elif" [toTree e, toTree b]

    toTree (While cond block) = Node "While" [toTree cond, toTree block]

    toTree (For init cond step block) = Node "For" [toTree init, toTree cond, toTree step, toTree block]

    toTree (Exp expr) = Node "Expr" [toTree expr] -- Isolated Expression

-- Expr

instance ToTree Expr where
    -- Isolated Expression
    toTree (e1 := e2)  = Node "(=)" [toTree e1, toTree e2]

    -- And & Or
    toTree (e1 :||: e2) = Node "(||)" [toTree e1, toTree e2]
    toTree (e1 :&&: e2) = Node "(&&)" [toTree e1, toTree e2]

    -- Comparison
    toTree (e1 :==: e2) = Node "(==)" [toTree e1, toTree e2]
    toTree (e1 :!=: e2) = Node "(!=)" [toTree e1, toTree e2]
    toTree (e1 :<: e2)  = Node "(<)" [toTree e1, toTree e2]
    toTree (e1 :<=: e2) = Node "(<=)" [toTree e1, toTree e2]
    toTree (e1 :>: e2)  = Node "(>)" [toTree e1, toTree e2]
    toTree (e1 :>=: e2) = Node "(>=)" [toTree e1, toTree e2]

    -- Arithmetical
    toTree (e1 :+: e2) = Node "(+)" [toTree e1, toTree e2]
    toTree (e1 :-: e2) = Node "(-)" [toTree e1, toTree e2]
    toTree (e1 :*: e2) = Node "(*)" [toTree e1, toTree e2]
    toTree (e1 :/: e2) = Node "(/)" [toTree e1, toTree e2]

    -- Unitary
    toTree (Not e) = Node "(Not)" [toTree e]
    toTree (Neg e) = Node "(Neg)" [toTree e]

    toTree (PostInc e) = Node "(++)" [toTree e]
    toTree (PostDec e) = Node "(--)" [toTree e]

    -- Unitary
    toTree (Var id) = Node ("Var: " ++ id) []
    toTree (e :.: id) = Node ("Field (." ++ id ++ ")") [toTree e] -- Struct Field Access
    toTree (e1 :@: e2) = Node "Array ([])" [toTree e1, toTree e2] -- Array Position Access

    toTree (FuncCall expr args) = Node "Call" (toTree expr : map toTree args)
    toTree (NewObj id fields) = Node ("New Obj: " ++ id) $ map toTree fields
    toTree (NewArray type_ dims) = Node "New Array" $ toTree type_ : map toTree dims

    toTree (LitInt i) = Node ("Lit Int: " ++ show i) []
    toTree (LitFloat f) = Node ("Lit Float: " ++ show f) []
    toTree (LitString s) = Node ("Lit String: " ++ show s) []
    toTree (LitBool b) = Node ("Lit Bool: " ++ show b) []
    toTree (LitArray exprs) = Node "LitArray" $ map toTree exprs -- To print literal arrays [1, 2, 3, 4]
    
    -- To print (2 + 3) from 1 + (2 + 3)
    toTree (Paren e) = Node "(expr)" [toTree e]
