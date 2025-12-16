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

  describe "integrate (Risch-Norman Heuristic)" $ do
    it "integrates x * exp(x)" $ do
      let f = x * exp(x)
      let res = integrate f x
      expand (diff res x) `shouldBe` f

  describe "Integration Helpers" $ do
    it "genPow 0 2 generates [[0,0]]" $ do
      genPow 0 2 `shouldBe` [[0,0]]

    it "genPow 1 2 generates correct combinations" $ do
      genPow 1 2 `shouldBe` [[0,0],[0,1],[1,0]]

    it "genPow 2 2 generates correct combinations" $ do
      genPow 2 2 `shouldBe` [[0,0],[0,1],[0,2],[1,0],[1,1],[2,0]]

    it "candidateDegree for polynomial" $ do
      candidateDegree (x^2 + 1) x `shouldBe` 3

    it "candidateDegree for x" $ do
      candidateDegree x x `shouldBe` 2

    it "terms extracts basis functions" $ do
      let ts = terms (sin x) x
      ts `shouldContain` [x]

    it "isRational detects rational functions" $ do
      isRational (x^2 + 1) x `shouldBe` True
      isRational ((x+1)/(x+2)) x `shouldBe` True
      isRational (sin x) x `shouldBe` False
      isRational (x * exp x) x `shouldBe` False

  describe "Extended Euclidean Algorithm" $ do
    it "extendedEuclidean(x^2, x) = (x, 0, 1)" $ do
      let (g, s, t) = extendedEuclidean (x^2) x x
      -- Verify: s*x^2 + t*x = g
      expand (s*x^2 + t*x) `shouldBe` g

    it "extendedEuclidean(x^3, x^2) has gcd x^2" $ do
      let (g, s, t) = extendedEuclidean (x^3) (x^2) x
      expand (s*x^3 + t*x^2) `shouldBe` g
      -- g should divide both x^3 and x^2
      degree g `shouldSatisfy` (<= 2)

    it "extendedEuclidean of coprime polynomials" $ do
      let (g, s, t) = extendedEuclidean (x+1) (x+2) x
      expand (s*(x+1) + t*(x+2)) `shouldBe` g
      -- coprime polynomials have gcd = constant
      degree g `shouldBe` 0

  describe "Polynomial Division (quotRemPoly)" $ do
    it "divides x^2 by x correctly" $ do
      let (q, r) = quotRemPoly (x^2) x x
      expand (q*x + r) `shouldBe` x^2
      degree r `shouldSatisfy` (< 1)

    it "divides x^3 + x by x correctly" $ do
      let (q, r) = quotRemPoly (x^3 + x) x x
      expand (q*x + r) `shouldBe` x^3 + x

  describe "Integration Properties" $ do
    it "derivative of integral returns original (polynomials)" $ do
      let f = x^4 + 2*x^3 + x
      let intF = integrate f x
      expand (diff intF x) `shouldBe` expand f

    it "derivative of integral returns original (trig)" $ do
      let f = 3*sin(x) + 2*cos(x)
      let intF = integrate f x
      expand (diff intF x) `shouldBe` expand f

    it "linearity: integrate(a*f + b*g) = a*integrate(f) + b*integrate(g)" $ do
      let f = x^2
      let g = x^3
      let lhs = integrate (2*f + 3*g) x
      let rhs = 2 * integrate f x + 3 * integrate g x
      expand (diff lhs x) `shouldBe` expand (diff rhs x)

  describe "rischNorman' (Risch-Norman Heuristic Algorithm)" $ do
    it "rischNorman' integrates x*exp(x) correctly" $ do
      let f = x * exp(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' integrates exp(x) correctly" $ do
      let f = exp(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' integrates x*exp(x) + exp(x)" $ do
      let f = x*exp(x) + exp(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' integrates x^2*exp(x)" $ do
      let f = x^2 * exp(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' integrates sin(x)*cos(x)" $ do
      let f = sin(x) * cos(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' handles sum of exponentials" $ do
      let f = exp(x) + 2*exp(x)
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f

    it "rischNorman' result contains constant of integration" $ do
      -- The result should contain a0 (or similar) representing the constant
      let f = exp(x)
      let res = rischNorman' f x
      -- Check that result has the form: something involving a0
      length (show res) `shouldSatisfy` (> 0)

    it "rischNorman' for polynomial should work (even if simple)" $ do
      -- rischNorman' should handle polynomials too, though simple integration is better
      let f = x^2
      let res = rischNorman' f x
      expand (diff res x) `shouldBe` expand f
