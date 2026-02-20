# SL Language Compiler

A compiler for the **SL Language** developed in **Haskell** as part of the **Compiler Construction I (BCC328)** course at the Federal University of Ouro Preto (UFOP).

This project implements the frontend phases (Lexical Analysis, Syntactic Analysis) and includes a Pretty Printer and an Abstract Syntax Tree (AST) visualizer.

## Features

* **Lexical Analysis**: Token generation using **Alex**.
* **Syntactic Analysis**: Parsing and grammar validation using **Happy**.
* **Abstract Syntax Tree (AST)**: Generation and visualization of the syntax tree.
* **Pretty Printing**: Reconstructs the source code from the AST.

## Prerequisites

To build and run this project, you need:

* **GHC** (Glasgow Haskell Compiler)
* **Cabal** (Build system)
* **Alex** (Lexical analyser generator)
* **Happy** (Parser generator)

## Usage

The project creates an executable named `sl`. You can run it via `cabal run`.

### Command Syntax

```bash
cabal run sl -- [OPTION] [FILE]
```

> Note: The double dash `--` is required to separate Cabal flags from program arguments

### Available Options

| Flag | Long Flag | Description |
| :--- | :--- | :--- |
| **`-l`** | `--lexer` | Runs the **Lexer** and prints the list of tokens. |
| **`-pt`** | `--parser` | Runs the **Parser** and prints the **Abstract Syntax Tree (AST)**. |
| **`-pp`** | `--pretty` | Runs the Parser and **Pretty Prints** the formatted source code. |
| **`-h`** | `--help` | Shows the help message. |

**Examples:**

```bash
cabal run sl -- --lexer input.sl
cabal run sl -- --parser input.sl
cabal run sl -- --pretty input.sl
```

## Testing

The project includes a test suite defined in `sl_lang.cabal`. To run the automated tests (HUnit):

```bash
cabal run sl-tests
```

## Project Structure

```text
.
├── app/
│   └── Main.hs             # CLI Entry Point (Args parsing & Main execution)
├── src/
│   ├── Frontend/
│   │   ├── Lexer/
│   │   │   ├── SL.x        # Alex lexer definition file
│   │   │   ├── SL.hs       # Generated lexer code (from Alex)
│   │   │   └── Token.hs    # Token data type definitions
│   │   └── Parser/
│   │       ├── SL.y        # Happy parser definition file (Grammar)
│   │       ├── SL.hs       # Generated parser code (from Happy)
│   │       ├── SL.info     # Happy debugging info (states and conflicts)
│   │       └── Syntax.hs   # Abstract Syntax Tree (AST) definitions
│   └── Utils/
│       ├── Pretty.hs       # Pretty printer implementation
│       └── Tree.hs         # AST visualization utilities
├── test/
│   ├── Main.hs             # Test suite entry point
│   ├── Lexer.hs            # Lexer unit tests
│   ├── Parser.hs           # Parser unit tests
│   └── Samples/            # Source code examples (.sl) for testing
│       ├── ex1.sl
│       ├── ...
│       └── ex6.sl
├── sl_lang.cabal           # Cabal project configuration
└── README.md               # Project documentation
```

## Authors

  * **Pedro Augusto Sousa Gonçalves**
  * **Gabriel Carlos Silva**