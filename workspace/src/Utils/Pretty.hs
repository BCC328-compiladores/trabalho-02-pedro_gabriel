module Utils.Pretty where

import Prelude hiding ((<>))
import Frontend.Syntax
import Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

pretty :: Pretty a => a -> String
pretty = render . pp

printPretty :: SL -> IO ()
printPretty = putStrLn . pretty

-- Main & Type

instance Pretty SL where
    pp (SL decls) = vcat $ map pp decls

instance Pretty Type where
    pp TyInt = text "int"
    pp TyFloat = text "float"
    pp TyString = text "string"
    pp TyBool = text "bool"
    pp TyVoid = text "void"
    pp (TyStruct id) = text id
    pp (TyVar id) = text id
    pp (TyArray t Nothing) = pp t <> text "[]"
    pp (TyArray t (Just e)) = pp t <> text "[" <> pp e <> text "]" 
    pp (TyFunc args ret) = 
        text "(" <> hsep (punctuate (text ",") (map pp args)) <> text ")" <+> text "->" <+> pp ret

-- Decl (Struct / Func)

instance Pretty Decl where
    pp (Struct id fields) = 
        text "struct" <+> text id <+> text "{" $$ 
        nest 4 ( vcat (map pp fields) ) $$ 
        text "}"
    
    pp (Func generics id params retType block) = 
        pp generics <+> text "func" <+> text id <>
        text "(" <> hsep (punctuate (text ",") (map pp params)) <> text ")" <+> 
        ppRet retType <+> text "{" $$
        nest 4 (pp block) $$
        text "}"

instance Pretty Field where
    pp (Field id type_) = text id <+> text ":" <+> pp type_ <> text ";"

instance Pretty Generics where
    pp (Generics Nothing) = empty
    pp (Generics (Just ids)) = text "forall" <+> hsep (map text ids) <+> text "."

instance Pretty Param where
    pp (Param id Nothing) = text id
    pp (Param id (Just t)) = text id <+> text ":" <+> pp t

-- Helper to handle optional return types
ppRet :: Maybe Type -> Doc
ppRet Nothing = empty
ppRet (Just t) = text ":" <+> pp t <+> empty

-- Stmt

instance Pretty Block where
    pp (Block stmts) = vcat $ map pp stmts

instance Pretty Stmt where
    pp (VarDecl id type_ init) = 
        text "let" <+> text id <+> ppTypeAnnot type_ <+> 
        maybe empty (\e -> text "=" <+> pp e) init <> text ";"
      where
        -- Handle type inference
        ppTypeAnnot Nothing = empty
        ppTypeAnnot (Just t) = text ":" <+> pp t
    
    pp Continue = 
        text "continue" <> text ";"
 
    pp Break = 
        text "break" <> text ";"

    pp (Return expr) = 
        text "return" <+> maybe empty pp expr <> text ";"
    
    pp (Print expr) = 
        text "print" <> text "(" <> pp  expr <> text ")" <> text ";"

    pp (Scan expr) = 
        text "scan" <> text "(" <> pp  expr <> text ")" <> text ";"    

    pp (IF cond block elifs elseBlock) = 
        text "if" <+> text "(" <> pp  cond <> text ")" <+> text "{" $$
        nest 4 (pp block) $$
        text "}" $$
        vcat (map ppElif elifs) $$ -- Print elifs if have
        ppElse elseBlock -- Print eles if have
      where 
        ppElif (e, b) = text "elif" <+> text "(" <> pp e <> text ")" <+> text "{" $$ nest 4 (pp b) $$ text "}"
        ppElse Nothing = empty
        ppElse (Just b) = text "else" <+> text "{" $$ nest 4 (pp b) $$ text "}"

    pp (While cond block) = 
        text "while" <+> text "(" <> pp cond <> text ")" <+> text "{" $$
        nest 4 (pp block) $$
        text "}"

    pp (For init cond step block) = 
        text "for" <+> text "(" <> pp init <> text ";" <+> pp cond <> text ";" <+> pp step <>  text ")" <+> text "{" $$
        nest 4 (pp block) $$
        text "}"
    
    -- Isolated Expression
    pp (Exp expr) = 
        pp expr <> text ";"

-- Expr

instance Pretty Expr where
    -- Assignment
    pp (e1 := e2) = pp e1 <+> text "=" <+> pp e2

    -- And & Or
    pp (e1 :||: e2) = pp e1 <+> text "||" <+> pp e2
    pp (e1 :&&: e2) = pp e1 <+> text "&&" <+> pp e2
    
    -- Comparison
    pp (e1 :==: e2) = pp e1 <+> text "==" <+> pp e2
    pp (e1 :!=: e2) = pp e1 <+> text "!=" <+> pp e2
    pp (e1 :<: e2)  = pp e1 <+> text "<" <+> pp e2
    pp (e1 :<=: e2)  = pp e1 <+> text "<=" <+> pp e2
    pp (e1 :>: e2)  = pp e1 <+> text ">" <+> pp e2
    pp (e1 :>=: e2)  = pp e1 <+> text ">=" <+> pp e2
    
    -- Arithmetical
    pp (e1 :+: e2) = pp e1 <+> text "+" <+> pp e2
    pp (e1 :-: e2) = pp e1 <+> text "-" <+> pp e2
    pp (e1 :*: e2) = pp e1 <+> text "*" <+> pp e2
    pp (e1 :/: e2) = pp e1 <+> text "/" <+> pp e2
    
    -- Unitary
    pp (Not e) = text "!" <> pp e
    pp (Neg e) = text "-" <> pp e

    pp (PostInc e) = pp e <> text "++"
    pp (PostDec e) = pp e <> text "--"
    
    -- Others
    pp (Var id) = text id
    pp (e :.: id) = pp e <> text "." <> text id             -- Struct Field Access
    pp (e1 :@: e2) = pp e1 <> text "[" <> pp e2 <> text "]" -- Array Position Access
    
    pp (FuncCall expr args) = pp expr <> text "(" <> hsep (punctuate comma (map pp args)) <> text ")"
    pp (NewObj id args) = text id <> text "{" <> hsep (punctuate comma (map pp args)) <> text "}"
    pp (NewArray t dims) = text "new" <+> pp t <> hcat (map (\d -> text "[" <> pp d <> text "]") dims)
    
    pp (LitInt i) = int i
    pp (LitString s) = doubleQuotes (text s)
    pp (LitFloat f) = float f
    pp (LitBool b) = text (if b then "true" else "false")
    -- To print literal arrays [1, 2, 3, 4]
    pp (LitArray exprs) = text "[" <> hsep (punctuate (text ",") (map pp exprs)) <> text "]"

    -- To print 1 + (2 + 3)
    pp (Paren e) = text "(" <> pp e <> text ")"
