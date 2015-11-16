{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Simplify where

import Algebra.CAS.Type

simp :: Value -> Value
simp (C a :+: C b) = C (a+b)
simp (CI a :+: CI b) = CI (a+b)
simp (x :+: C 0) = simp x
simp (x :+: CI 0) = simp x
simp (C 0 :+: x) = simp x
simp (CI 0 :+: x) = simp x
simp (x :+: y) = 
  case (simp x,simp y) of
    (CI 0,CI 0) -> CI 0
    (CI 0,y') -> y'
    (x',CI 0) -> x'
    (x',y') -> x' :+: y'
    
simp (C a :*: C b) = C (a*b)
simp (CI a :*: CI b) = CI (a*b)
simp (_ :*: C 0) = CI 0
simp (x :*: C 1) = simp x
simp (_ :*: CI 0) = CI 0
simp (x :*: CI 1) = simp x
simp (C 0 :*: _) = CI 0
simp (C 1 :*: x) = simp x
simp (CI 0 :*: _) = CI 0
simp (CI 1 :*: x) = simp x
simp (x :*: y) =
  case (simp x,simp y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> CI 0
    (x',y') -> x' :*: y'


simp (C a :/: C b) = C (a/b)
simp (CI a :/: CI b) = CI (a `div` b)
simp (_ :/: C 0) = error "divide by 0"
simp (x :/: C 1) = simp x
simp (_ :/: CI 0) = error "divide by 0"
simp (x :/: CI 1) = simp x
simp (C 0 :/: _) = CI 0
simp (C 1 :/: x) = CI 1 :/: simp x
simp (CI 0 :/: _) = CI 0
simp (CI 1 :/: x) = CI 1 :/: simp x
simp (x :/: y) =
  case (simp x,simp y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> error "divide by 0"
    (x',y') -> x' :/: y'

simp a = a
