module Parser where

import Test.HUnit
import Frontend.Parser.SL (parseSL)
import Frontend.Syntax
import Frontend.Lexer.SL (runAlex)

parse :: String -> Either String SL
parse input = runAlex input parseSL

-- ###############################
-- Test Decls (Structs / Func)
-- ###############################

-- Test Struct

testStruct :: Test
testStruct = TestCase $ do
    let code = "struct Point{x:int;y:float;}"
    let expected = SL [Struct "Point" [Field "x" TyInt, Field "y" TyFloat]]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Struct Parse:" expected ast

-- Test All Func Possibilities

testFuncSimpleVoid :: Test
testFuncSimpleVoid = TestCase $ do
    let code = "func main():void{}"
    let expected = SL [Func (Generics Nothing) "main" [] (Just TyVoid) (Block [])]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Func Simple Parse:" expected ast

testFuncTypedNoReturn :: Test
testFuncTypedNoReturn = TestCase $ do
    let code = "func add(a:int,b:float){}"
    let params = [Param "a" (Just TyInt), Param "b" (Just TyFloat)]
    let expected = SL [Func (Generics Nothing) "add" params Nothing (Block [])]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Func Typed Parse:" expected ast

testFuncGenerics :: Test
testFuncGenerics = TestCase $ do
    let code = "forall T . func generic(x:(T)->T):T{}"
    
    let gen = Generics (Just ["T"])
    let tType = TyStruct "T" 
    let params = [Param "x" $ Just $ TyFunc [tType] tType]
    let ret = Just tType
    
    let expected = SL [Func gen "generic" params ret (Block [])]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Func Generics Parse:" expected ast

testFuncInferredParams :: Test
testFuncInferredParams = TestCase $ do
    let code = "func map(x, y) {}"

    let params = [Param "x" Nothing, Param "y" Nothing]
    let expected = SL [Func (Generics Nothing) "map" params Nothing (Block [])]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Func Inferred Params Parse:" expected ast

testFuncWithBody :: Test
testFuncWithBody = TestCase $ do
    let code = "func main(){print(1);return;}"
    
    let body = [ Print (LitInt 1)
               , Return Nothing 
               ]
    let expected = SL [Func (Generics Nothing) "main" [] Nothing (Block body)]
    
    case parse code of
        Left err -> assertFailure err
        Right ast -> assertEqual "Func Body Parse:" expected ast

declTests :: Test
declTests = TestList [
      TestLabel "Struct Definition" testStruct
    , TestLabel "Func Simple Return Void" testFuncSimpleVoid
    , TestLabel "Func Typed No Return" testFuncTypedNoReturn
    , TestLabel "Func Generics" testFuncGenerics
    , TestLabel "Func Inferred" testFuncInferredParams
    , TestLabel "Func Body" testFuncWithBody
    ]

-- ###############################
-- Test Stmt
-- ###############################

-- Stmt Helper: wrap the stmt inside a 'func main() { expr; }' to use the main parser, and parse a single stmt
parseStmt :: String -> Either String [Stmt]
parseStmt bodyCode = 
    let fullCode = "func main() { " ++ bodyCode ++ " }"
    in case runAlex fullCode parseSL of
        Right (SL [Func _ _ _ _ (Block stmts)]) -> Right stmts
        Right _ -> Left "Unexpected structure (not a main function)"
        Left err -> Left err

-- Var Decl
testVarDecl :: Test
testVarDecl = TestCase $ do
    -- Type and Value
    let code1 = "let x: int = 10;"
    let exp1  = [VarDecl "x" (Just TyInt) (Just (LitInt 10))]
    
    -- Type Only
    let code2 = "let y : float;"
    let exp2  = [VarDecl "y" (Just TyFloat) Nothing]
    
    -- Inference
    let code3 = "let z = true;"
    let exp3  = [VarDecl "z" Nothing (Just (LitBool True))]

    case (parseStmt code1, parseStmt code2, parseStmt code3) of
        (Right r1, Right r2, Right r3) -> do
            assertEqual "VarDecl Complete" exp1 r1
            assertEqual "VarDecl Type Only"  exp2 r2
            assertEqual "VarDecl Inferred" exp3 r3
        errs -> assertFailure $ "Error in VarDecl: " ++ show errs



-- If + Elif + Else
testIfElse :: Test
testIfElse = TestCase $ do
    let code = "if (x > 0) { print(1); } elif (x < 0) { print(2); } else { print(0); }"
    
    let condIf   = Var "x" :>: LitInt 0
    let blockIf  = Block [Print (LitInt 1)]
    
    let condElif = Var "x" :<: LitInt 0
    let blockElif = Block [Print (LitInt 2)]
    
    let blockElse = Just (Block [Print (LitInt 0)])
    
    let expected = [IF condIf blockIf [(condElif, blockElif)] blockElse]
    
    case parseStmt code of
        Right res -> assertEqual "If-Elif-Else Parsing" expected res
        Left err  -> assertFailure err

-- While and For
testLoops :: Test
testLoops = TestCase $ do
    -- While
    let codeWhile = "while(true) { x++; }"
    let expWhile  = [While (LitBool True) (Block [Exp (PostInc (Var "x"))])]
    
    -- For
    let codeFor   = "for(i=0; i<10; i++) { print(i); }"
    let init      = Var "i" := LitInt 0
    let cond      = Var "i" :<: LitInt 10
    let step      = PostInc (Var "i")
    let blockFor  = Block [Print (Var "i")]
    let expFor    = [For init cond step blockFor]

    case (parseStmt codeWhile, parseStmt codeFor) of
        (Right rW, Right rF) -> do
            assertEqual "While Parsing" expWhile rW
            assertEqual "For Parsing"   expFor   rF
        err -> assertFailure $ "Error in Loops: " ++ show err

-- Return, Print, Scan, Exp
testOthers :: Test
testOthers = TestCase $ do
    -- Return empty and with value
    let codeRet = "return; return 5;"
    let expRet  = [Return Nothing, Return (Just (LitInt 5))]
    
    -- Print
    let codePrint = "print(\"Hello\");"
    let expPrint  = [Print (LitString "Hello")]
    
    -- Scan
    let codeScan = "scan(a);"
    let expScan  = [Scan (Var "a")]
    
    -- Isolated Exp (Function call)
    let codeExp = "doSomething();"
    let expExp  = [Exp (FuncCall (Var "doSomething") [])]

    case parseStmt (codeRet ++ codePrint ++ codeExp) of
        Right stmts -> assertEqual "Other Stmts" (expRet ++ expPrint ++ expExp) stmts
        Left err -> assertFailure err


stmtTests :: Test
stmtTests = TestList [
      TestLabel "Var Declarations" testVarDecl
    , TestLabel "If/Elif/Else"     testIfElse
    , TestLabel "Loops (While/For)" testLoops
    , TestLabel "Return/Print/Exp"  testOthers
    ]

-- ###############################
-- Test Expr
-- ###############################

-- Expr Helper: wrap the expr inside a 'func main() { expr; }' to use the main parser, and parse a single expr
parseExpr :: String -> Either String Expr
parseExpr exprCode = 
    let fullCode = "func main() { " ++ exprCode ++ "; }"
    in case runAlex fullCode parseSL of
        -- Extracts the expression inside the block -> Exp expr
        Right (SL [Func _ _ _ _ (Block [Exp e])]) -> Right e
        Right _ -> Left "Unexpected structure (not a main function with one expr)"
        Left err -> Left err

-- Arithmetic & Logic Precedence Tests
testPrecedence :: Test
testPrecedence = TestCase $ do
    -- Struct (.) -> (*) -> (+)
    let codeMath = "2 + 3 * a.b"
    let expMath  = LitInt 2 :+: (LitInt 3 :*: (Var "a" :.: "b"))

    -- (&&) -> (||)
    let codeLogic = "x || y && z"
    let expLogic  = Var "x" :||: (Var "y" :&&: Var "z")

    -- Array (@) -> (==) -> (&&)
    let codeComp = "x == y && a != b[2]"
    let expComp  = (Var "x" :==: Var "y") :&&: (Var "a" :!=: (Var "b" :@: LitInt 2))

    -- Assignment is Right Associative
    let codeAsg = "x = y = z"
    let expAsg  = Var "x" := (Var "y" := Var "z")

    case (parseExpr codeMath, parseExpr codeLogic, parseExpr codeComp, parseExpr codeAsg) of
        (Right r1, Right r2, Right r3, Right r4) -> do
            assertEqual "Math Precedence (* over +)" expMath r1
            assertEqual "Logic Precedence (&& over ||)" expLogic r2
            assertEqual "Comparison Precedence" expComp r3
            assertEqual "Assignment Associativity" expAsg r4
        errs -> assertFailure $ "Error in Binary Precedence: " ++ show errs

-- Unary & Postfix Tests
testUnary :: Test
testUnary = TestCase $ do
    -- Negation and Not
    let code1 = "-x + !y"
    let exp1  = Neg (Var "x") :+: Not (Var "y")

    -- Post-Increment/Decrement
    let code2 = "x++ + y--"
    let exp2  = PostInc (Var "x") :+: PostDec (Var "y")

    case (parseExpr code1, parseExpr code2) of
        (Right r1, Right r2) -> do
            assertEqual "Unary Operators" exp1 r1
            assertEqual "Postfix Operators" exp2 r2
        err -> assertFailure $ "Error in Unary: " ++ show err

-- Struct of Structs & Multidimensional Arrays
testComplexAccess :: Test
testComplexAccess = TestCase $ do
    -- Struct of Structs 
    let codeStruct = "person.address.city"
    let expStruct  = (Var "person" :.: "address") :.: "city"

    -- Multidimensional Array Access
    let codeArray  = "matrix[i][j]"
    let expArray   = (Var "matrix" :@: Var "i") :@: Var "j"

    -- Mixed: Struct Array
    let codeMixed  = "users[0].name"
    let expMixed   = (Var "users" :@: LitInt 0) :.: "name"

    case (parseExpr codeStruct, parseExpr codeArray, parseExpr codeMixed) of
        (Right r1, Right r2, Right r3) -> do
            assertEqual "Nested Struct Access" expStruct r1
            assertEqual "Multidimensional Array" expArray r2
            assertEqual "Mixed Access" expMixed r3
        err -> assertFailure $ "Error in Complex Access: " ++ show err

-- Object/Array Creation & Func Call Tests
testCreations :: Test
testCreations = TestCase $ do
    -- Function Call
    let codeCall = "sum(a, 10)"
    let expCall  = FuncCall (Var "sum") [Var "a", LitInt 10]

    -- Object Creation
    let codeObj  = "Point { 10, 20 }"
    let expObj   = NewObj "Point" [LitInt 10, LitInt 20]

    -- New Multidimensional Array
    let codeNewArr = "new int[5][10]"
    let expNewArr  = NewArray TyInt [LitInt 5, LitInt 10]

    -- Array Literal
    let codeLitArr = "[1, 2, 3]"
    let expLitArr  = LitArray [LitInt 1, LitInt 2, LitInt 3]

    case (parseExpr codeCall, parseExpr codeObj, parseExpr codeNewArr, parseExpr codeLitArr) of
        (Right r1, Right r2, Right r3, Right r4) -> do
            assertEqual "Function Call" expCall r1
            assertEqual "New Object"    expObj  r2
            assertEqual "New Array"     expNewArr r3
            assertEqual "Array Literal" expLitArr r4
        err -> assertFailure $ "Error in Creations: " ++ show err


exprTests :: Test
exprTests = TestList [
      TestLabel "Precedence and Operations" testPrecedence
    , TestLabel "Unary/Postfix"     testUnary
    , TestLabel "Complex Access (Struct/Array)" testComplexAccess
    , TestLabel "Creations (New/Call)" testCreations
    ]

parserTests :: Test
parserTests = TestList [
      declTests
    , stmtTests
    , exprTests
    ]
