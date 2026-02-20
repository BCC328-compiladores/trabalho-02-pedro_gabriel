# SL Language Compiler

A compiler and interpreter for the **SL Language** developed in **Haskell** as part of the **Compiler Construction I (BCC328)** course at the **Federal University of Ouro Preto (UFOP)**.

This project implements **frontend**, **semantic analysis**, and **execution** phases of a compiler, including:

* Lexical and syntactic analysis
* Abstract Syntax Tree (AST) visualization
* Pretty Printer
* Static type checking
* Interpreter
* Interactive REPL

---

## Features

### Frontend

* **Lexical Analysis** using **Alex**
* **Syntactic Analysis** using **Happy**
* **AST Generation** with tree visualization
* **Pretty Printing** from AST back to formatted source code

### Semantic Analysis

* **Static Type Checker**

  * Variable, function, and struct validation
  * Function signatures and generics
  * Detailed error reporting with line and column

### Execution

* **Interpreter**

  * Executes complete `.sl` programs
* **REPL (Read–Eval–Print Loop)**

  * Interactive execution of statements
  * Supports declarations (`func`, `struct`, `forall`)
  * Type checking before execution
  * Persistent environment and symbol tables

---

## Prerequisites

To build and run this project, you need:

* **GHC** (Glasgow Haskell Compiler)
* **Cabal**
* **Alex** (Lexical analyzer generator)
* **Happy** (Parser generator)

---

## Building the Project

```bash
cabal build
```

---

## Usage

The project creates an executable named `sl`.

```bash
cabal run sl -- [OPTION] [FILE]
```

> ⚠️ The double dash `--` is required to separate Cabal flags from program arguments.

---

## Command-Line Options

|  Flag | Long Flag       | Description                                     |
| ----: | --------------- | ----------------------------------------------- |
|  `-l` | `--lexer`       | Runs the **Lexer** and prints the token stream  |
| `-pt` | `--parser`      | Runs the **Parser** and prints the **AST**      |
| `-pp` | `--pretty`      | Pretty prints the source code from the AST      |
| `-tc` | `--typecheck`   | Runs **semantic analysis** (type checking only) |
|  `-i` | `--interpreter` | Executes an SL program                          |
|  `-r` | `--repl`        | Starts the interactive **REPL**                 |
|  `-h` | `--help`        | Shows the help message                          |

---

### Examples

```bash
cabal run sl -- --lexer input.sl
cabal run sl -- --parser input.sl
cabal run sl -- --pretty input.sl
cabal run sl -- --typecheck input.sl
cabal run sl -- --interpreter input.sl
cabal run sl -- --repl
```

---

## REPL Mode

By default (or using `--repl`), the compiler starts an interactive shell:

```text
=================================================
 Welcome to REPL SL!
 Type ':q' or ':quit' to exit.
=================================================
SL>
```

### REPL Features

* Supports:

  * Expressions
  * Statements
  * Function, struct, and generic declarations
* Performs **syntax + semantic analysis** before execution
* Maintains:

  * Type context
  * Global declarations
  * Runtime environment

---

## Grammar

The full language grammar is documented in:

```
../grammar.md
```

It includes:

* Structs
* Functions with generics
* Arrays and function types
* Control flow (`if`, `elif`, `else`, `while`, `for`)
* Expressions, operators, and literals
* type inferece

---

## Testing

The project includes an automated test suite using **HUnit**.

```bash
cabal run tests
```

### Test Coverage

* Lexer unit tests
* Parser unit tests
* Sample SL programs

---

## Project Structure

```text
.
├── app/
│   └── Main.hs             # CLI, REPL, Interpreter entry point
├── src/
│   ├── Frontend/
│   │   ├── Syntax.hs  # AST definitions
│   │   ├── Lexer/
│   │   ├── Parser/
│   │   └── Semantics/
│   ├── Interpreter.hs
│   └── Utils/
│       ├── Pretty.hs      # Pretty printer
│       └── Tree.hs        # AST visualization
├── test/
│   ├── Main.hs
│   ├── Lexer.hs
│   ├── Parser.hs
│   └── Samples/
│       ├── Assigment/
│       ├── Force_bounds/
│       ├── Force_errors/
│       └── Ed1/
├── sl_lang.cabal
└── README.md
```

---

## Authors

* **Pedro Augusto Sousa Gonçalves**
* **Gabriel Carlos Silva**
