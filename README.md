# th-cas : Compile-time CAS(Computer Algebra System)

[![Hackage version](https://img.shields.io/hackage/v/th-cas.svg?style=flat)](https://hackage.haskell.org/package/th-cas)  [![Build Status](https://travis-ci.org/junjihashimoto/th-cas.png?branch=master)](https://travis-ci.org/junjihashimoto/th-cas)

CAS(Computer Algebra System) operations like diff are done at compile-time.
(integrate-function is under development.)

## Usage

Write ```diff function var``` with Quotes of TemplateHaskell.
```diff``` makes a diffirential of the function at compile-time.
So there is no overhead at execution-time.

A example is below.

```
import Algebra.CAS.TH

myfunc x = $(diff [|x*x+2*x+1|] [|x|])
```
