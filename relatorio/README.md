# Compiler Construction I - Final Report

This repository contains the source code for the documentation and final report of the Compiler Construction I course (**BCC328**) at the Federal University of Ouro Preto (UFOP).

## Dependencies

To compile this project successfully, you need the following system tools and LaTeX packages.

### System Tools
* **GNU Make**: To automate the build process.
* **Latexmk**: To manage LaTeX compilation dependencies automatically.
* **TeX Distribution**: Any standard distribution like TeX Live (Linux), MacTeX (macOS), or MiKTeX (Windows).

### Key LaTeX Packages
The following packages are required within the LaTeX environment (usually included in `texlive-latex-extra`):

* `babel` (option `brazil`): For Portuguese language support and hyphenation.
* `inputenc` / `fontenc`: For UTF-8 character encoding.
* `listings` or `minted`: For syntax highlighting of the compiler source code (Lexer/Parser rules).
* `graphicx`: To include images (e.g., AST diagrams).
* `geometry`: For page margin configuration.
* `hyperref`: For clickable links in the table of contents and reference

## How to Compile

A `Makefile` is included to automate the build process. Open your terminal in the project root directory and use the following commands:

| Command | Description |
| :--- | :--- |
| **`make`** | Compiles the source code and generates the `main.pdf`. |
| **`make view`** | Opens the generated PDF using the system's default viewer. |
| **`make clean`** | Removes temporary files (`.aux`, `.log`, `.toc`, etc.). |
| **`make distclean`** | Removes temporary files AND the generated PDF. |

### Troubleshooting

If you encounter a **"Clock skew detected"** warning, run the following command to reset file timestamps:

```bash
find . -exec touch {} +
```

## Project Structure

```text
.
├── Makefile             # Build automation script
├── README.md            # This file
├── main.tex             # Main LaTeX entry point
└── sections/            # Report chapters
    ├── Arquitetura/     # Lexical, AST, Grammar
    ├── Metodologia/     # Syntax, Type Inference, Semantics
    ├── Testes/          # Test cases and limitations
    ├── intro.tex        # Introduction
    └── Conclusão.tex    # Final remarks
```

## Authors

  * **Pedro Augusto Sousa Gonçalves** - Matrícula: 21.1.4015
  * **Gabriel Carlos Silva** - Matrícula: 23.1.4016