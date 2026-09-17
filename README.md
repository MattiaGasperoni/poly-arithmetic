# poly-arithmetic

Polynomial arithmetic in two paradigms — **Haskell** (functional) and **Prolog** (logic) — implementing the same algorithm from the same formal specification, so the two implementations can be compared side by side.

Developed as the final project for the *Logic and Functional Programming* course, Computer Science, University of Urbino Carlo Bo (A.Y. 2025/2026).

## What it does

Each program reads two single-variable, real-coefficient polynomials from standard input and computes:

- their canonical algebraic representation
- their **degree**
- their **sum**
- their **difference**
- their **product**
- the **quotient and remainder** of their Euclidean division
- their **GCD** (greatest common divisor), via the Euclidean algorithm, returned in monic form

## Input format

Coefficients are typed on a single line, separated by spaces or tabs, listed in **increasing order of degree** (the value at position *i* is the coefficient of *xⁱ*). At least one coefficient is required; the zero polynomial is entered as `0`.

- Decimal separator: `.`
- Scientific notation allowed: `1e-3`, `1.5E2`
- Hexadecimal/octal notation is **not** allowed

```
Enter the coefficients of polynomial A (increasing degree order): 1 2 3
```
represents `1 + 2x + 3x²`, printed back as `3x^2 + 2x + 1`.

## Design highlights

- **Representation:** a polynomial is a list of coefficients in increasing degree order; the zero polynomial is the empty list.
- **Numerical tolerance:** a threshold of ε = 10⁻⁶ absorbs floating-point rounding noise when comparing coefficients to zero or to one.
- **Normalization:** trailing near-zero coefficients are dropped so the leading coefficient is always meaningfully non-zero (or the list is empty), keeping degree computation robust.
- **Output formatting:** canonical form from highest to lowest degree, omitting zero terms, unit coefficients, and the `x⁰`/`x¹` powers; non-integer coefficients are rounded to 4 decimal places.

See `Relazione.pdf` for the full problem specification, design rationale, and a detailed comparison of the two implementations (input validation, recursion style, division algorithm, output rendering), plus 10 test cases run against both programs.

## Files

| File | Description |
|---|---|
| `operazioni_polinomi.hs` | Haskell implementation |
| `operazioni_polinomi.pl` | Prolog implementation |
| `Relazione.pdf` | Full project report (Italian) |

## Running

### Haskell

Requires GHC.

```bash
ghc -o poly operazioni_polinomi.hs
./poly
```

### Prolog

Requires GNU Prolog.

```bash
gprolog --consult-file operazioni_polinomi.pl --entry-goal main
```

## Authors

- Mattia Gasperoni
- Alessio Cavalieri

## License

Academic project — see repository for license terms.
