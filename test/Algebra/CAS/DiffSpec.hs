{-# LANGUAGE OverloadedStrings #-}

module Algebra.CAS.DiffSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Base
import Algebra.CAS.Diff

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
  describe "diff" $ do
    it "diff(x+y,x)" $ do
      diff (x+y) (x) `shouldBe` 1
    it "diff(x*x+y,x)" $ do
      diff (x*x+y) (x) `shouldBe` 2*x
    it "diff(sin(x*x)+y+z,x)" $ do
      diff (sin(x*x)+y+z) (x) `shouldBe` 2*cos(x*x)*x
    it "diff(log(x),x)" $ do
      diff (log(x)) (x) `shouldBe` 1/x
    it "diff(tan(x),x)" $ do
      diff (tan(x)) (x) `shouldBe` 1 + tan(x)**2
    it "diff(exp(x),x)" $ do
      diff (exp(x)) (x) `shouldBe` exp(x)
    it "diff(x^5,x)" $ do
      diff (x^5) (x) `shouldBe` 5*x^4

  describe "diffn (nth derivative)" $ do
    it "diffn 0 f x = f" $ do
      diffn 0 (x^2) x `shouldBe` x^2
    it "diffn 1 f x = diff f x" $ do
      diffn 1 (x^2) x `shouldBe` diff (x^2) x
    it "diffn 2 (x^3) x = 6*x" $ do
      diffn 2 (x^3) x `shouldBe` 6*x
    it "diffn 3 (x^3) x = 6" $ do
      diffn 3 (x^3) x `shouldBe` 6
    it "diffn 4 (x^3) x = 0" $ do
      diffn 4 (x^3) x `shouldBe` 0
    it "diffn 2 (sin(x)) x = -sin(x)" $ do
      diffn 2 (sin(x)) x `shouldBe` -sin(x)
    it "diffn 3 (exp(x)) x = exp(x)" $ do
      diffn 3 (exp(x)) x `shouldBe` exp(x)
