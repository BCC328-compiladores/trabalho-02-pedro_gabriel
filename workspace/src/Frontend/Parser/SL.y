{
module Frontend.Parser.SL where

import Frontend.Lexer.Token
import Frontend.Lexer.SL hiding (lexer)
import Frontend.Syntax
}

%name parseSL Main
%name parseDecl Decl
%name parseStmt Stmt
%name parseExpr Expr

%monad {Alex} { (>>=) } { return } 
%tokentype { Token }
%error { parseError }
%lexer { lexer } { Token _ TkEOF }

%token 
    struct      { Token _ TkStruct }
    continue    { Token _ TkContinue }
    break       { Token _ TkBreak }
    forall      { Token _ TkForAll }
    func        { Token _ TkFunc }
    let         { Token _ TkLet }
    return      { Token _ TkReturn }
    print       { Token _ TkPrint }
    scan        { Token _ TkScan }
    if          { Token _ TkIF }
    elif        { Token _ TkElif }
    else        { Token _ TkElse }
    while       { Token _ TkWhile }
    for         { Token _ TkFor }
    new         { Token _ TkNew }
    
    int         { Token _ TkInt }
    float       { Token _ TkFloat }
    string      { Token _ TkString }
    bool        { Token _ TkBool }
    void        { Token _ TkVoid }

    id          { Token _ (TkID $$) }
    int_lit     { Token _ (TkIntLit $$) }
    float_lit   { Token _ (TkFloatLit $$) }
    str_lit     { Token _ (TkStringLit $$) }
    bool_lit    { Token _ (TkBoolLit $$) }

    '('         { Token _ TkLParen }
    ')'         { Token _ TkRParen }
    '['         { Token _ TkLBracket }
    ']'         { Token _ TkRBracket }
    '{'         { Token _ TkLBrace }
    '}'         { Token _ TkRBrace }
    '.'         { Token _ TkDot }
    ','         { Token _ TkComma }
    ':'         { Token _ TkColon }
    ';'         { Token _ TkSemiColon }
    '->'        { Token _ TkArrow }

    '++'        { Token _ TkInc }
    '--'        { Token _ TkDec }
    '='         { Token _ TkAsg }
    '=='        { Token _ TkEq }
    '!='        { Token _ TkNotEq }
    '<'         { Token _ TkLss }
    '<='        { Token _ TkLssEq }
    '>'         { Token _ TkGtr }
    '>='        { Token _ TkGtrEq }
    '&&'        { Token _ TkAnd }
    '||'        { Token _ TkOr }
    '!'         { Token _ TkNot }
    '+'         { Token _ TkAdd }
    '-'         { Token _ TkSub }
    '*'         { Token _ TkMul }
    '/'         { Token _ TkDiv }

-- Precedence
%nonassoc LOW -- Minimal precedence
%right '->'
%right '='
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%left NEG   -- ex: -1
%left '!'
%left '.' '[' ']'

%%


Main :: { SL }
    : DeclList { SL $1 }

DeclList :: { [Loc Decl] }
    : Decl DeclList { $1 : $2 }
    |               { [] }

Decl :: { Loc Decl }
    : StructDef { $1 }
    | FuncDef   { $1 }

-- Struct

StructDef :: { Loc Decl }
    : struct id '{' FieldList '}' { Loc (getPos $1) (Struct $2 $4) }

FieldList :: { [Loc Field] }
    : FieldDecl FieldList { $1 : $2 }
    |                     { [] }

FieldDecl :: { Loc Field }
    : id ':' Type ';' { Loc (getPos $2) (Field $1 $3) }

-- Func

FuncDef :: { Loc Decl }
    : GenericsDecl func id '(' ParamList ')' OptReturnType '{' Block '}' 
      { Loc (getPos $2) (Func $1 $3 $5 $7 $9) }

GenericsDecl :: { Generics }
    : forall IDList '.' { Generics (Just $2) }
    |                   { Generics Nothing }

IDList :: { [ID] }
    : id IDList { $1 : $2 }
    | id        { [$1] }

ParamList :: { [Loc Param] }
    : Param ',' ParamList { $1 : $3 }
    | Param               { [$1] }
    |                     { [] }

Param :: { Loc Param }
    : id ':' Type         { Loc (getPos $2) (Param $1 (Just $3)) }
    | id                  { Loc (0, 0) (Param $1 Nothing) }

OptReturnType :: { Maybe Type }
    : ':' Type    { Just $2 }
    |             { Nothing }

-- Type

Type :: { Type }
    : BaseType                   { $1 }
    | Type '[' ']'               { TyArray $1 Nothing }
    | Type '[' Expr ']'          { TyArray $1 (Just $3)  }
    | '(' TypeList ')' '->' Type { TyFunc $2 $5 } 

-- BaseType is a helper rule for Array Creation to avoid shift reduce error 
-- (distinguishes between recursive 'Type[]' and the 'DimList' in 'new int[10]')

BaseType :: { Type }
    : int                        { TyInt }
    | float                      { TyFloat }
    | string                     { TyString }
    | bool                       { TyBool }
    | void                       { TyVoid }
    | id                         { TyStruct $1 }

TypeList :: { [Type] }
    : Type ',' TypeList { $1 : $3 }
    | Type              { [$1] }
    |                   { [] }

-- Stmt

Block :: { Block }
    : StmtList { Block $1 }

StmtList :: { [Loc Stmt] }
    : Stmt StmtList { $1 : $2 }
    |               { [] }

Stmt :: { Loc Stmt }
    -- Create a var
    : let id ':' Type '=' Expr ';'   { Loc (getPos $1) (VarDecl $2 (Just $4) (Just $6)) }
    | let id ':' Type ';'            { Loc (getPos $1) (VarDecl $2 (Just $4) (Nothing)) }
    | let id '=' Expr ';'            { Loc (getPos $1) (VarDecl $2 (Nothing) (Just $4)) }

    | return OptExpr ';'             { Loc (getPos $1) (Return $2) } -- return
    | print '(' Expr ')' ';'         { Loc (getPos $1) (Print $3) }  -- print
    | scan  '(' Expr ')' ';'         { Loc (getPos $1) (Scan $3) }   -- scan
    | IfStmt                         { $1 }        -- IfStmt will now return a Loc Stmt
    | continue ';'                   { Loc (getPos $1) Continue }  
    | break ';'                      { Loc (getPos $1) Break }
    | WhileStmt                      { $1 }        -- while
    | ForStmt                        { $1 }        -- for 
    | Expr ';'                       { Loc (0, 0) (Exp $1) }    -- Dummy position for isolated Expr. 

-- If has something to return
OptExpr :: { Maybe Expr }
    : Expr        { Just $1 }
    |             { Nothing }

-- If -> Maybe [Elif] -> Maybe Else

IfStmt :: { Loc Stmt }
    : if '(' Expr ')' '{' Block '}' ElifList OptElse 
      { Loc (getPos $1) (IF $3 $6 $8 $9) }

ElifList :: { [(Expr, Block)] }
    : elif '(' Expr ')' '{' Block '}' ElifList  { ($3, $6) : $8 }
    |                                           { [] }

OptElse :: { Maybe Block }
    : else '{' Block '}'  { Just $3 }
    |                     { Nothing }

-- While

WhileStmt :: { Loc Stmt }
    : while '(' Expr ')' '{' Block '}' { Loc (getPos $1) (While $3 $6) }

-- For

ForStmt :: { Loc Stmt }
    : for '(' Expr ';' Expr ';' Expr ')' '{' Block '}' { Loc (getPos $1) (For $3 $5 $7 $10) }

-- Expr

Expr :: { Expr }
    -- Assignment
    : LValue '=' Expr               { $1 := $3 }

    -- And & Or
    | Expr '||' Expr                { $1 :||: $3 }
    | Expr '&&' Expr                { $1 :&&: $3 }

    -- Comparison
    | Expr '==' Expr                { $1 :==: $3 }
    | Expr '!=' Expr                { $1 :!=: $3 }
    | Expr '<'  Expr                { $1 :<:  $3 }
    | Expr '<=' Expr                { $1 :<=: $3 }
    | Expr '>'  Expr                { $1 :>:  $3 }
    | Expr '>=' Expr                { $1 :>=: $3 }

    -- Arithmetical
    | Expr '+' Expr                 { $1 :+: $3 }
    | Expr '-' Expr                 { $1 :-: $3 }
    | Expr '*' Expr                 { $1 :*: $3 }
    | Expr '/' Expr                 { $1 :/: $3 }

    -- Unitary
    | '!' Expr                      { Not $2 }
    | '-' Expr %prec NEG            { Neg $2 }     -- "%prec NEG": Force the precedence of NEG passing previously

    | LValue '++'                   { PostInc $1 } -- x++
    | LValue '--'                   { PostDec $1 } -- X--

    -- Others
    | LValue                        { $1 }
    | FuncCall                      { $1 }
    | ObjCreation                   { $1 }

    | Literals                      { $1 }
    | ArrayCreation                 { $1 }
    
    | '(' Expr ')'                  { Paren $2 }

LValue :: { Expr }
    : id                            { Var $1 }
    | Expr '.' id                   { $1 :.: $3 }    -- Struct Field Access
    | Expr '[' Expr ']'             { $1 :@: $3 }    -- Array Position Access

FuncCall :: { Expr }
    : id '(' ExprList ')'           { FuncCall (Var $1) $3 }

ObjCreation :: { Expr }
    : id '{' ExprList '}'           { NewObj $1 $3 }

ArrayCreation :: { Expr }
    : new BaseType DimList         { NewArray $2 $3 } -- Uses BaseType to avoid shift reduce error

-- To multidimensional arrays 
DimList :: { [Expr] }
    : '[' Expr ']' DimList         { $2 : $4}
    | '[' Expr ']' %prec LOW       { [$2] }

Literals :: { Expr }
    : int_lit          { LitInt $1 }
    | float_lit        { LitFloat $1 }
    | str_lit          { LitString $1 }
    | bool_lit         { LitBool $1 }
    | '[' ExprList ']' { LitArray $2 } -- Array Literal

ExprList :: { [Expr] }
    : Expr ',' ExprList { $1 : $3 }
    | Expr              { [$1] }
    |                   { [] }

{

-- Helper to get the token position
getPos :: Token -> Pos
getPos t = case pos t of
    (l, c) -> (l, c)

-- Error handling function called by Happy when parsing fails.
parseError :: Token -> Alex a
parseError t = do
    let (line, col) = pos t 
    alexError $ "Parse error at line " ++ show line 
             ++ ", column " ++ show col 
             ++ ": Unexpected token " ++ show (lexeme t)

-- It invokes 'alexMonadScan' to retrieve the next token and passes it to the parser's continuation function.
lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}
