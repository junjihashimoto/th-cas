{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.CAS.BasicSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Type

main :: IO ()
main = hspec spec

x :: Value
x = "x"
y :: Value
y = "y"
z :: Value
z = "z"

spec :: Spec
spec = do
  describe "simplify(sqrt)" $ do
    it "sqrt(4) == 2" $ do
      let v = 4 :: Value
      sqrt v `shouldBe` 2
    it "sqrt(0) == 0" $ do
      let v = 0 :: Value
      sqrt v `shouldBe` 0
    it "sqrt(1) == 1" $ do
      let v = 1 :: Value
      sqrt v `shouldBe` 1
    it "sqrt(3) == Sqrt 3" $ do
      let v = 3 :: Value
      sqrt v `shouldBe` S (Sqrt 3)
    it "sqrt(-1) == I" $ do
      let v = -1 :: Value
      sqrt v `shouldBe` I
    it "I*I == -1" $ do
      I*I `shouldBe` -1
  describe "add" $ do
    it "x<y" $ do
      x<y `shouldBe` True
    it "y<x" $ do
      y<x `shouldBe` False
    it "x<x**2" $ do
      x<x**2 `shouldBe` True
    it "x>**2" $ do
      x>x**2 `shouldBe` False
    it "x+y==y+x" $ do
      x+y `shouldBe` y+x
    it "x+y+y==x+2*y" $ do
      x+y+y `shouldBe` x+2*y
    it "x**2+x**2+y == 2*x**2+y" $ do
      x**2+x**2+y `shouldBe` 2*x**2+y
    it "2*y+x+x+y == 2*x+3*y" $ do
      2*y+x+x+y+z `shouldBe` 2*x+3*y+z
  describe "mult" $ do
    it "x*x==x**2" $ do
      x*x `shouldBe` x**2
    it "x*x**2==x**3" $ do
      x*x**2 `shouldBe` x**3
    it "x**0=1" $ do
      x**0 `shouldBe` 1
    it "(x+y)*y != x*y+y**2" $ do
      (x+y)*y ==  x*y+y**2 `shouldBe` False
    it "expand ((x+y)*y) == x*y+y**2" $ do
      expand ((x+y)*y) `shouldBe` x*y+y**2 
  describe "div" $ do
    it "x/x==1" $ do
      x/x `shouldBe` 1
    it "x*y/x == y" $ do
      x*y/x `shouldBe` y
    it "y*x**2*z/x == x*y*z" $ do
      y*x**2*z/x `shouldBe` x*y*z
    it "(x**2+x)/x == (x**2+x):/:x" $ do
      (x**2+x)/x `shouldBe` (x**2+x):/:x
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
--    it "subst x 1 (x^2+x+3) = 5" $ do
--      subst [(x,1)] (x**2+x+3) `shouldBe` 5
