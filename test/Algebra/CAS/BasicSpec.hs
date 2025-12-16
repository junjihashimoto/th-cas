{-# LANGUAGE OverloadedStrings #-}

module Algebra.CAS.BasicSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Base

main :: IO ()
main = hspec spec

x :: Formula
x = "x"
y :: Formula
y = "y"
z :: Formula
z = "z"

spec :: Spec
spec = do
  describe "simplify(sqrt)" $ do
    it "sqrt(4) == 2" $ do
      let v = 4 :: Formula
      sqrt v `shouldBe` 2
    it "sqrt(0) == 0" $ do
      let v = 0 :: Formula
      sqrt v `shouldBe` 0
    it "sqrt(1) == 1" $ do
      let v = 1 :: Formula
      sqrt v `shouldBe` 1
    it "sqrt(3) == Sqrt 3" $ do
      let v = 3 :: Formula
      sqrt v `shouldBe` S (Sqrt 3)
    it "sqrt(-1) == I" $ do
      let v = -1 :: Formula
      sqrt v `shouldBe` I
    it "I*I == -1" $
      I*I `shouldBe` -1
  describe "add" $ do
    it "x<y" $
      x<y `shouldBe` True
    it "y<x" $
      y<x `shouldBe` False
    it "x<x**2" $
      x<x**2 `shouldBe` True
    it "x>**2" $
      x>x**2 `shouldBe` False
    it "x+y==y+x" $
      x+y `shouldBe` y+x
    it "x+y+y==x+2*y" $
      x+y+y `shouldBe` x+2*y
    it "x**2+x**2+y == 2*x**2+y" $
      x**2+x**2+y `shouldBe` 2*x**2+y
    it "2*y+x+x+y == 2*x+3*y" $
      2*y+x+x+y+z `shouldBe` 2*x+3*y+z
  describe "mult" $ do
    it "x*x==x**2" $
      x*x `shouldBe` x**2
    it "x*x**2==x**3" $
      x*x**2 `shouldBe` x**3
    it "x**0=1" $
      x**0 `shouldBe` 1
    it "(x+y)*y != x*y+y**2" $
      (x+y)*y ==  x*y+y**2 `shouldBe` False
    it "expand ((x+y)*y) == x*y+y**2" $
      expand ((x+y)*y) `shouldBe` x*y+y**2 
    it "expand (1+x)*(2+x)*((y+x)*(y+x*2)+x)" $
      expand ((1+x)*(2+x)*((y+x)*(y+x*2)+x)) `shouldBe` 2*x + 7*(x^2) + 7*(x^3) + 2*(x^4) + 6*x*y + 9*(x^2)*y + 3*(x^3)*y + 2*(y^2) + 3*x*(y^2) + (x^2)*(y^2)
  describe "div" $ do
    it "x/x==1" $
      x/x `shouldBe` 1
    it "x*y/x == y" $
      x*y/x `shouldBe` y
    it "y/x*x == y" $
      (y/x)*x `shouldBe` y
    it "y*x**2*z/x == x*y*z" $
      y*x**2*z/x `shouldBe` x*y*z
    it "(x**2+x)/x == (x**2+x):/:x" $
      (x**2+x)/x `shouldBe` (x**2+x):/:x
    it "-(x+y-1)+x-y-3 == -2y-2" $
      expand (x-y-3-(x+y-1)) `shouldBe` -2*y-2
--    it " ((2*x)/(y))*(-1)*y == -2x" $ do
--      expand (((2*x)/(y))*(-1)*y) `shouldBe` -2*x
  describe "gcd" $ do
    it "gcdPolynomial (2*x) x == x " $
      gcdPolynomial (2*x) x `shouldBe`  x
    it "gcdPolynomial (expand $ (x+y)*(x+2)) (expand $ (x+y)*(x+3)) == (x+y)" $ do
      gcdPolynomial (expand $ (x+y)*(x+2)) (expand $ (x+y)*(x+3)) `shouldBe` (x+y)
  describe "lcm" $ do
    it "lcmPolynomial  (expand $ (x+y)*(x+1)) (expand $ (x+y)*(x+2))" $ do
      lcmPolynomial  (expand $ (x+y)*(x+1)) (expand $ (x+y)*(x+2)) `shouldBe` expand ((x+y)*(x+1)*(x+2))
  describe "read and show" $ do
    it "showFormula" $ do
      showFormula  (x^2+x+y) `shouldBe` "((V \"x\" :+: (V \"x\" :^: C (CI 2))) :+: V \"y\")"
    it "read Formula" $ do
      read (showFormula  (x^2+x+y)) `shouldBe` x^2+x+y
    it "pretty print" $ do
      show (x^2+x+y) `shouldBe` "x + x^2 + y"
  describe "substitute" $ do
    it "subst [(x,1),(y,2)] (x+y) = 3" $ do
      subst [(x,1),(y,2)] (x+y) `shouldBe` 3

  describe "Const operations (Bug Fixes)" $ do
    it "signum One should be CI 1 (bug fix verification)" $ do
      signum (One :: Const) `shouldBe` CI 1
    it "signum Zero should be CI 0" $ do
      signum (Zero :: Const) `shouldBe` CI 0
    it "signum (CI 5) should be CI 1" $ do
      signum (CI 5 :: Const) `shouldBe` CI 1
    it "signum (CI (-3)) should be CI (-1)" $ do
      signum (CI (-3) :: Const) `shouldBe` CI (-1)

  describe "degree" $ do
    it "degree of constant is 0" $ do
      degree (5 :: Formula) `shouldBe` 0
    it "degree of x is 1" $ do
      degree x `shouldBe` 1
    it "degree of x^3 is 3" $ do
      degree (x^3) `shouldBe` 3
    it "degree of x^2 + y^3 is 3" $ do
      degree (x^2 + y^3) `shouldBe` 3
    it "degree of x*y is 1" $ do
      degree (x*y) `shouldBe` 1

  describe "numer and denom" $ do
    it "numer of 1/x is 1" $ do
      numer (1/x) `shouldBe` 1
    it "denom of 1/x is x" $ do
      denom (1/x) `shouldBe` x
    it "numer of (x+1)/(x+2) is x+1" $ do
      numer ((x+1)/(x+2)) `shouldBe` x+1
    it "denom of (x+1)/(x+2) is x+2" $ do
      denom ((x+1)/(x+2)) `shouldBe` x+2
    it "numer of polynomial is itself" $ do
      numer (x^2 + x + 1) `shouldBe` x^2 + x + 1
    it "denom of polynomial is 1" $ do
      denom (x^2 + x + 1) `shouldBe` 1
