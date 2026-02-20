module Semantic where

import Test.HUnit
import Frontend.Lexer.SL (runAlex)
import Frontend.Parser.SL (parseSL)
import Frontend.Semantics.StmtTC (tychProgram)
import Frontend.Semantics.Basics (ErrorTypes(..))
import Frontend.Syntax (Type(..))

-- ==========================================
-- Execution Helper
-- ==========================================

-- Runs the complete pipeline and returns Success (Right ()) 
-- or a list of formatted Error Strings (Left [String])
runSemantic :: String -> Either [String] ()
runSemantic code =
    case runAlex code parseSL of
        Left err -> Left ["Parser Error: " ++ err]
        Right ast -> case tychProgram ast of
                        Right () -> Right ()
                        Left errs -> Left (map (show . fst) errs) -- Extracts the ErrorType and ignores the Pos

-- ==========================================
-- Success Tests (Valid Programs)
-- ==========================================

testValidPrograms :: Test
testValidPrograms = TestCase $ do
    -- Simple Program
    let codeSimple = "func main() : void { let x: int = 10; print(x); }"
    assertEqual "Valid Simple Main" (Right ()) (runSemantic codeSimple)

    -- Polymorphism / Generics (Identity)
    let codePoly = "func id(x) { return x; }\nfunc main() { let a: int = id(5); }"
    assertEqual "Valid Polymorphism" (Right ()) (runSemantic codePoly)

    -- New For Loop with Scope
    let codeFor = "func main() { for(let i: int = 0; i < 10; i++) { print(i); } }"
    assertEqual "Valid For Loop Scope" (Right ()) (runSemantic codeFor)

    -- Structs and Arrays
    let codeStruct = "struct P { x: int; } func main() { let p = P{1}; let arr = new int[5]; arr[0] = p.x; }"
    assertEqual "Valid Struct and Array" (Right ()) (runSemantic codeStruct)


-- ==========================================
-- Basic Errors and Main Validation
-- ==========================================

testMainValidation :: Test
testMainValidation = TestCase $ do
    -- Missing main function (Assuming you added this check to your compiler)
    let codeNoMain = "func foo() {}"
    assertEqual "Program without main is valid" (Right ()) (runSemantic codeNoMain)

    -- Main with parameters
    let codeMainArgs = "func main(a: int) {}"
    let expMainArgs = Left [show $ InvalidMainSignature "The 'main' function cannot have parameters or generics."]
    assertEqual "Main with arguments" expMainArgs (runSemantic codeMainArgs)

    -- Duplicate declaration
    let codeDup = "func main() {} func main() {}"
    let expDup = Left [show $ DuplicateDecl "main"]
    assertEqual "Duplicate function" expDup (runSemantic codeDup)


-- ==========================================
-- Static Typing Tests
-- ==========================================

testTypeErrors :: Test
testTypeErrors = TestCase $ do
    -- Incompatible assignment
    let codeAssign = "func main() { let x: int = \"hello\"; }"
    let expAssign = Left [show $ IncompatibleTypes TyInt TyString]
    assertEqual "Assign String to Int" expAssign (runSemantic codeAssign)

    -- Undefined variable
    let codeUndef = "func main() { y = 10; }"
    let expUndef = Left [show $ UndefinedVariable "y"]
    assertEqual "Undefined Variable" expUndef (runSemantic codeUndef)

    -- If condition is not boolean
    let codeIfBool = "func main() { if (1) { print(1); } }"
    let expIfBool = Left [show $ IncompatibleTypes TyBool TyInt]
    assertEqual "If condition not Bool" expIfBool (runSemantic codeIfBool)

    -- Invalid Math Operation
    let codeMath = "func main() { let x = 5 - \"test\"; }"
    let expMath = Left [show $ GenericError "Cannot subtract, multiply, or divide Int and String"]
    assertEqual "Invalid Math Operation" expMath (runSemantic codeMath)

-- ==========================================
-- Control Flow and Functions Tests
-- ==========================================

testControlFlowAndFuncs :: Test
testControlFlowAndFuncs = TestCase $ do
    -- Break outside a loop
    let codeBreak = "func main() { break; }"
    let expBreak = Left [show $ BreakOutsideLoop "Break outside of a loop"]
    assertEqual "Break outside loop" expBreak (runSemantic codeBreak)

    -- Missing Return
    let codeMissingRet = "func foo(): int {} func main() {}"
    let expMissingRet = Left [show $ MissingReturn "foo"]
    assertEqual "Missing Return" expMissingRet (runSemantic codeMissingRet)

    -- Arity Mismatch (Too many parameters)
    let codeArity = "func sum(a: int) {} func main() { sum(1, 2); }"
    let expArity = Left [show $ ArityMismatch "<function>" 1 2]
    assertEqual "Arity Mismatch" expArity (runSemantic codeArity)


-- ==========================================
-- Data Structures Tests
-- ==========================================

testDataStructures :: Test
testDataStructures = TestCase $ do
    -- Field access on a non-struct variable
    let codeNotStruct = "func main() { let x: int = 10; print(x.y); }"
    let expNotStruct = Left [show $ NotAStruct TyInt]
    assertEqual "Field access on Int" expNotStruct (runSemantic codeNotStruct)

    -- Non-existent field in struct
    let codeUndefField = "struct A { x: int; } func main() { let a = A{1}; print(a.y); }"
    let expUndefField = Left [show $ UndefinedField "A" "y"]
    assertEqual "Undefined Field" expUndefField (runSemantic codeUndefField)

    -- Array with non-integer index
    let codeArrIdx = "func main() { let arr = new int[5]; arr[\"zero\"] = 1; }"
    let expArrIdx = Left [show $ IncompatibleTypes TyInt TyString] 
    assertEqual "Array index not Int" expArrIdx (runSemantic codeArrIdx)


-- ==========================================
-- Grouping and Execution
-- ==========================================

semanticTests :: Test
semanticTests = TestList [
      TestLabel "Valid Programs"        testValidPrograms
    , TestLabel "Main & Basics"         testMainValidation
    , TestLabel "Type Checking"         testTypeErrors
    , TestLabel "Flow & Functions"      testControlFlowAndFuncs
    , TestLabel "Data Structures"       testDataStructures
    ]

-- To run in GHCi or via Cabal
runAllSemanticTests :: IO Counts
runAllSemanticTests = runTestTT semanticTests
