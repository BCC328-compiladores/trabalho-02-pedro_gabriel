# SL Language Grammar

Main -> DeclList

DeclList -> Decl DeclList | $\lambda$

Decl -> StructDef | FuncDef

## Struct Definition:
StructDef -> "struct" *ID* "{" FieldList "}"

FieldList -> FieldDecl FieldList | $\lambda$

FieldDecl -> *ID* ":" Type ";"

## Function Definition:
FuncDef -> GenericsDecl "func" *ID* "(" ParamList ")" OptReturnType "{" Block "}"

GenericsDecl -> "forall" IDList "." | $\lambda$

IDList -> *ID* IDList | *ID*

ParamList -> Param "," ParamList | Param | $\lambda$

Param -> *ID* ":" Type | *ID*

OptReturnType -> ":" Type | $\lambda$

## Types:
Type ->  "int" | "float" | "string" | "bool" | "void" | *ID* | Type "[" "]" | Type "[" Expr "]" | "(" TypeList ")" "->" Type

TypeList -> Type "," TypeList | Type | $\lambda$

## Statements:
Block -> StmtList

StmtList -> Stmt StmtList | $\lambda$

Stmt -> VarDecl ";" | ReturnStmt ";" | PrintStmt ";" | Expr ";" | IfStmt | WhileStmt | ForStmt

VarDecl -> "let" *ID* ":" Type "=" Expr |  "let" *ID* ":" Type | "let" *ID* "=" Expr

ReturnStmt -> "return" Expr | "return" 

PrintStmt -> "print" "(" Expr ")" 

IfStmt -> "if" "(" Expr ")" "{" Block "}" OptElif 

OptElif -> "elif" "(" Expr ")" "{" Block "}" OptElif | OptElse 

OptElse -> "else" "{" Block "}" | $\lambda$ 

WhileStmt -> "while" "(" Expr ")" "{" Block "}" 

ForStmt -> "for" "(" Expr ";" Expr ";" Expr ")" "{" Block "}"

## Expressions:
Expr -> AssignExpr 

AssignExpr -> LValue "=" OrExpr | OrExpr 

OrExpr -> OrExpr "||" AndExpr | AndExpr 

AndExpr -> AndExpr "&&" CompExpr | CompExpr 

CompExpr ->  AddExpr CompOp AddExpr | AddExpr 

AddExpr -> AddExpr AddOp MultiExpr | MultiExpr 

MultiExpr -> MultiExpr MultiOp UnaryExpr | UnaryExpr 

UnaryExpr -> "!" UnaryExpr | "-" UnaryExpr | PrimaryExpr 

PrimaryExpr -> Literal | "[" ExprList "]" | LValue | FuncCall | ObjCreation | ArrayCreation | "(" Expr ")" | IncrementExpr | DecrementExpr

LValue -> *ID* | LValue "[" Expr "]" | LValue "." *ID* 

FuncCall -> *ID* "(" ExprList ")" 

ExprList -> Expr "," ExprList | Expr | $\lambda$ 

ObjCreation -> *ID* "{" ExprList "}" 

ArrayCreation -> *new* Type DimList

DimList -> "[" Expr "]" DimList | "[" Expr "]"

IncrementExpr -> LValue "++" 

DecrementExpr -> LValue "--"

## Auxiliary Terminals:
CompOp -> "==" | "!=" | "<" | "<=" | ">" | ">=" 

AddOp -> "+" | "-" 

MultiOp -> "*" | "/" 

Literal -> *IntLit* | *FloatLit* | *StringLit* | *BoolLit*