{-# LANGUAGE OverloadedStrings #-}

module Algebra.CAS.IntegrateSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Base
import Algebra.CAS.Integrate
import Algebra.CAS.Diff (diff)

main :: IO ()
main = hspec spec

x :: Formula
x = "x"

spec :: Spec
spec = do
  describe "integrate (Basic Polynomials)" $ do
    it "integrates constant: 5 -> 5*x" $ do
      let f = 5
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates x -> x^2/2" $ do
      let f = x
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates x^2 -> x^3/3" $ do
      let f = x^2
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates x^3 -> x^4/4" $ do
      let f = x^3
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates 3*x^2 -> x^3" $ do
      let f = 3*x^2
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates 2*x + 3 -> x^2 + 3*x" $ do
      let f = 2*x + 3
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates 3*x^2 + 2*x + 1" $ do
      let f = 3*x^2 + 2*x + 1
      let res = integrate f x
      expand (diff res x) `shouldBe` f

  describe "integrate (Trigonometric)" $ do
    it "integrates sin(x) -> -cos(x)" $ do
      let f = sin x
      let res = integrate f x
      expand (diff res x) `shouldBe` f

    it "integrates cos(x) -> sin(x)" $ do
      let f = cos x
      let res = integrate f x
      expand (diff res x) `shouldBe` f

  describe "integrate (Rational Functions)" $ do
    it "integrates 1/x -> log(x)" $ do
      let f = 1/x
      let res = integrate f x
      expand (diff res x) `shouldBe` f

  describe "Helper Functions" $ do
    it "Extended Euclidean: gcd(x^2, x) = x" $ do
      let (g, s, t) = extendedEuclidean (x^2) x x
      -- Verify: s*x^2 + t*x = g
      expand (s*x^2 + t*x) `shouldBe` g
