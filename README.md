# Poly-Arithmetic: Symbolic Multivariate Polynomial Engine

A dual-paradigm symbolic computation library for manipulating, simplifying, and performing algebraic operations on multivariate polynomials. Implemented in both **Prolog** (logic programming) and **Haskell** (purely functional programming) to explore structural pattern matching, algebraic data types, and term rewriting systems.

---

## Table of Contents
- [Overview](#overview)
- [Architecture & Data Representation](#architecture--data-representation)
- [Features & Supported Operations](#features--supported-operations)
- [Project Structure](#project-structure)
- [Getting Started](#getting-started)
  - [Haskell Environment (GHCi)](#haskell-environment-ghci)
  - [Prolog Environment (SWI-Prolog)](#prolog-environment-swi-prolog)
- [Design Notes & Canonicalization](#design-notes--canonicalization)

---

## Overview

`poly-arithmetic` provides an end-to-end symbolic engine capable of parsing raw algebraic expressions into structured internal representations, reducing them to a deterministic canonical normal form, and executing polynomial arithmetic and calculus operations.

By implementing identical mathematical specifications across two declarative paradigms, the project highlights:
- **Haskell**: Strong static typing, Algebraic Data Types (ADTs), typeclasses (`Eq`, `Ord`, `Show`), and composable higher-order transformations.
- **Prolog**: Unification, relational predicates, backtracking-free deterministic term reduction, and structural decomposition.

---

## Architecture & Data Representation

To ensure unambiguous equality checks and deterministic output, every polynomial expression is normalized into a **canonical form** ordered lexicographically by variable names and exponents:

1. **Variable (`Var`)**: Represented as an identifier and an integer exponent (x^n). Variables with n = 0 collapse to the multiplicative identity (1).
2. **Monomial (`Monomial`)**: A scalar coefficient paired with a sorted list of unique variables with non-zero exponents. Like variables are merged by summing their exponents; zero coefficients collapse the entire monomial to 0.
3. **Polynomial (`Poly`)**: A sorted list of non-zero monomials with distinct variable signatures. Like monomials are combined by summing their coefficients, and zero-coefficient terms are pruned.

---

## Features & Supported Operations

Both implementations expose a complete suite of symbolic manipulation routines:

* **Expression Parsing & Canonicalization**: Converts arbitrary nested arithmetic trees (`+`, `-`, `*`, `^`) into flattened, sorted, canonical polynomials.
* **Polynomial Arithmetic**:
  * Addition
  * Subtraction 
  * Multiplication 
* **Evaluation**: Multi-variable substitution and numerical evaluation given a mapping of variable identifiers to values.
* **Structural Inspection**:
  * Extraction of coefficients and distinct variables 
  * Extraction of sorted monomials
  * Minimum and maximum total polynomial degree 
* **Pretty-Printing**: Human-readable algebraic string formatting.

---

## Project Structure

```text
poly-arithmetic/
├── operazioni_polinomi.hs   # Functional implementation in Haskell
├── operazioni_polinomi.pl   # Logic implementation in Prolog
├── Relazione.pdf            # Technical specification and design report (Italian)
└── README.md                # Project documentation
```

---

## Getting Started

### Haskell Environment (GHCi)

**Prerequisites**: [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) installed on your system.

1. Launch GHCi and load the module:
   ```bash
   ghci operazioni_polinomi.hs
   ```
2. Parse and manipulate polynomials directly in the REPL.

### Prolog Environment (SWI-Prolog)

**Prerequisites**: [SWI-Prolog](https://www.swi-prolog.org/) installed on your system.

1. Start SWI-Prolog and consult the source file:
   ```bash
   swipl -s operazioni_polinomi.pl
   ```
2. Query the predicates interactively.

---

## Design Notes & Canonicalization

* **Zero-Elimination Invariant**: The zero polynomial is consistently represented as an empty monomial list (`Poly []` in Haskell, `poly([])` in Prolog) to avoid edge-case ambiguity during degree computation and structural equality checks.
* **Total Ordering**: Custom comparators enforce strict ordering across monomials (sorted first by total degree, then lexicographically by variable name and exponent), guaranteeing that `P1 == P2` holds if and only if `P1` and `P2` are algebraically equivalent.
* **Documentation**: For a deep dive into the formal grammar, edge-case handling, and algorithmic complexity, refer to `Relazione.pdf`.
