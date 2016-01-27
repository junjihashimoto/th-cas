# th-cas : Compile-time CAS(Computer Algebra System)

[![Hackage version](https://img.shields.io/hackage/v/th-cas.svg?style=flat)](https://hackage.haskell.org/package/th-cas)  [![Build Status](https://travis-ci.org/junjihashimoto/th-cas.png?branch=master)](https://travis-ci.org/junjihashimoto/th-cas)

CAS(Computer Algebra System) operations like diff are done at compile-time.
Compile time operations is done in TemplateHaskell.

## Interactive Usage

```th-cas``` supports interactive usage like mathematica.
To start repl, type "stack ghci".

Variables are defined by ```V``` of data-constructor.

A example of intertative usage is below.

```
$ stack ghci
...
*Algebra.CAS> 
*Algebra.CAS> let x = V "x" 
*Algebra.CAS> let y = V "y" 
*Algebra.CAS> x^2 + 2*x + x^2
2*x + 2*(x^2)
*Algebra.CAS> diff (x^2 + 2*x + x^2) x
2 + 4*x
*Algebra.CAS> linsolve [x+y=:2,x-y=:3]
Just [(x,5/2),(y,-1/2)]
*Algebra.CAS> subst [(x,1)] $ diff (x^2 + 2*x + x^2) x
6
```

## Usage at compile time

Write ```diff function var``` with Quotes of TemplateHaskell.
```diff``` makes a diffirential of the function at compile-time.
So there is no overhead at execution-time.

A example is below.

Write following code with quotes.

```
import Algebra.CAS.TH

myfunc :: Int -> Int
myfunc x = $(diff [|x*x+2*x+1|] [|x|])
```

At compile time, the quotes is translated to ```2*x+2```.

```
import Algebra.CAS.TH

myfunc :: Int -> Int
myfunc x = 2*x+2
```
