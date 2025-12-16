# th-cas : Compile-time CAS (Computer Algebra System)

[![Hackage version](https://img.shields.io/hackage/v/th-cas.svg?style=flat)](https://hackage.haskell.org/package/th-cas)  [![Build Status](https://travis-ci.org/junjihashimoto/th-cas.png?branch=master)](https://travis-ci.org/junjihashimoto/th-cas)

`th-cas` is a Computer Algebra System (CAS) for Haskell that performs symbolic mathematics operations at compile-time using Template Haskell. It provides both interactive usage (like Mathematica) and compile-time optimization for mathematical computations.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Interactive Usage](#interactive-usage)
  - [Basic Algebra](#basic-algebra)
  - [Differentiation](#differentiation)
  - [Integration](#integration)
  - [Equation Solving](#equation-solving)
  - [Gröbner Basis](#gröbner-basis)
- [Compile-time Usage with Template Haskell](#compile-time-usage-with-template-haskell)
- [API Overview](#api-overview)
- [Advanced Examples](#advanced-examples)
- [Supported Operations](#supported-operations)
- [Development](#development)
- [Contributing](#contributing)
- [References](#references)

## Features

- **Symbolic Differentiation**: Automatic differentiation of algebraic expressions
- **Symbolic Integration**: Integration using the Risch-Norman heuristic algorithm
- **Equation Solving**: Linear and polynomial equation solvers
- **Gröbner Basis**: Computation for multivariate polynomial systems
- **Formula Simplification**: Automatic algebraic simplification
- **Pattern Matching**: Match and extract coefficients from expressions
- **Compile-time Optimization**: Zero runtime overhead using Template Haskell

## Installation

Add `th-cas` to your project dependencies:

```bash
cabal install th-cas
# or with stack
stack install th-cas
```

## Interactive Usage

`th-cas` supports interactive usage similar to Mathematica. Start the REPL with:

```bash
stack ghci
# or
cabal repl
```

Variables are defined using the `V` data constructor or with `OverloadedStrings`:

### Basic Algebra

```haskell
*Algebra.CAS> :set -XOverloadedStrings
*Algebra.CAS> let x = "x" :: Formula
*Algebra.CAS> let y = "y" :: Formula

-- Automatic simplification
*Algebra.CAS> x^2 + 2*x + x^2
2*x + 2*(x^2)

-- Expansion
*Algebra.CAS> expand $ (x + 1) * (x + 2)
2 + 3*x + x^2
```

### Differentiation

```haskell
-- Basic differentiation
*Algebra.CAS> diff (x^2 + 2*x + x^2) x
2 + 4*x

-- Chain rule
*Algebra.CAS> diff (sin(x^2)) x
2*x*cos(x^2)

-- Substitution
*Algebra.CAS> subst [(x,1)] $ diff (x^2 + 2*x + x^2) x
6
```

### Integration

```haskell
-- Polynomial integration
*Algebra.CAS> integrate (x^2) x
(x^3)/3

-- Exponential integration
*Algebra.CAS> integrate (x * exp(x)) x
(-1)*e(x) + x*e(x)

-- Trigonometric integration
*Algebra.CAS> integrate (sin(x) * cos(x)) x
(sin(x)^2)/2
```

### Equation Solving

```haskell
-- Linear system
*Algebra.CAS> linsolve [x+y=:2, x-y=:3]
Just [(x,5/2),(y,(-1)/2)]

-- Polynomial equations
*Algebra.CAS> solve (x^2 - 5*x + 6) x
[2,3]
```

### Gröbner Basis

```haskell
-- Compute Gröbner basis for polynomial ideal
*Algebra.CAS> let f1 = x^2 + y
*Algebra.CAS> let f2 = x*y + 1
*Algebra.CAS> grobnerBasis [f1, f2]
[-- basis polynomials --]

-- Polynomial reduction
*Algebra.CAS> reductions (x^2 + x*y) [x + y]
(-1)*(x^2) + x
```

## Compile-time Usage with Template Haskell

`th-cas` can perform CAS operations at compile-time using Template Haskell, generating optimized code with **zero runtime overhead**.

### Compile-time Differentiation

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Algebra.CAS.TH

-- This function
myfunc :: Int -> Int
myfunc x = $(diff [|x*x + 2*x + 1|] [|x|])

-- Is compiled to this optimized code at compile-time:
-- myfunc x = 2*x + 2
```

The differentiation happens during compilation, so there's no symbolic math overhead at runtime.

### Pattern Matching

Extract coefficients from expressions:

```haskell
*Algebra.CAS> match [a*x^2 + b*x + c] [x^2 + 3*x + 4]
Just [(a,1),(b,3),(c,4)]
```

## API Overview

### Core Functions

| Function | Description | Example |
|----------|-------------|---------|
| `diff f x` | Differentiate `f` with respect to `x` | `diff (x^2) x` → `2*x` |
| `diffn n f x` | nth derivative | `diffn 2 (x^3) x` → `6*x` |
| `integrate f x` | Integrate `f` with respect to `x` | `integrate (x^2) x` → `x^3/3` |
| `expand f` | Expand expression | `expand ((x+1)^2)` → `1 + 2*x + x^2` |
| `simplify f` | Simplify expression | `simplify (x/x)` → `1` |
| `subst env f` | Substitute variables | `subst [(x,2)] (x^2)` → `4` |
| `solve f x` | Solve equation | `solve (x^2-1) x` → `[-1,1]` |
| `linsolve eqs` | Solve linear system | `linsolve [x+y=:1]` → `Just [...]` |
| `grobnerBasis fs` | Compute Gröbner basis | `grobnerBasis [x^2+y, x*y]` |
| `degree f` | Polynomial degree | `degree (x^3)` → `3` |
| `gcdPolynomial f g` | Polynomial GCD | `gcdPolynomial (x^2-1) (x-1)` → `x-1` |

## Advanced Examples

### Solving Systems of Equations

```haskell
-- Three equations, three unknowns
*Algebra.CAS> let eqs = [x + y + z =: 6, 2*x + y - z =: 1, x - y + z =: 2]
*Algebra.CAS> linsolve eqs
Just [(x,1),(y,2),(z,3)]
```

### Polynomial Division and GCD

```haskell
-- Greatest Common Divisor
*Algebra.CAS> gcdPolynomial (x^2 - 1) (x^3 - 1)
(-1) + x

-- LCM
*Algebra.CAS> lcmPolynomial (x^2 - 1) (x^3 - 1)
1 + x + x^2 + (-1)*(x^3) + (-1)*(x^4) + (-1)*(x^5)
```

### Complex Integration

```haskell
-- Integration by parts (handled automatically)
*Algebra.CAS> integrate (x^2 * exp(x)) x
2*e(x) + (-2)*x*e(x) + (x^2)*e(x)

-- Verify by differentiation
*Algebra.CAS> diff (integrate (x^2 * exp(x)) x) x
(x^2)*e(x)
```

## Supported Operations

### Arithmetic
- Addition, subtraction, multiplication, division
- Integer and rational exponents
- Automatic simplification

### Functions
- Trigonometric: `sin`, `cos`, `tan`
- Exponential: `exp`, `log`
- Power: `sqrt`, `**`

### Special Features
- **Risch-Norman Integration**: Heuristic algorithm for symbolic integration
- **Hermite Reduction**: Rational function integration
- **Square-free Factorization**: Polynomial factorization (Yun's algorithm)
- **Extended Euclidean Algorithm**: For polynomial GCD

## Development

### Running Tests

```bash
cabal test
```

The project includes comprehensive test coverage:
- 150+ test cases
- Property-based tests
- Integration verification tests

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues.

## License

See LICENSE file for details.

## References

- Risch-Norman Algorithm for symbolic integration
- Buchberger's Algorithm for Gröbner basis computation
