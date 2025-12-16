{-# LANGUAGE OverloadedStrings #-}

module Algebra.CAS.SolveSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Base
import Algebra.CAS.Solve

main :: IO ()
main = hspec spec

x :: Formula
x = "x"
y :: Formula
y = "y"
z :: Formula
z = "z"

a :: Formula
a = CV "a"
b :: Formula
b = CV "b"
c :: Formula
c = CV "c"

spec :: Spec
spec = do
  describe "match" $ do
    it "x**2 vs x**2" $ do
      match (x**2) (x**2) `shouldBe` Just []
    it "a*x**2 vs x**2" $ do
      match (a*x**2) (x**2) `shouldBe` Just [(a,1)]
    it "a*x**2+b*x vs x**2" $ do
      match (a*x**2+b*x) (x**2) `shouldBe` Nothing
    it "a*x**2+b*x vs x**2+3*x" $ do
      match (a*x**2+b*x) (x**2+3*x) `shouldBe` Just [(b,3),(a,1)]
    it "a*x**2+b*x+c vs x**2+3*x" $ do
      match (a*x**2+b*x+c) (x**2+3*x) `shouldBe` Just [(c,0),(b,3),(a,1)]
    it "a*x**2+b*x+c vs x**2+3*x+4" $ do
      match (a*x**2+b*x+c) (x**2+3*x+4) `shouldBe` Just [(c,4),(b,3),(a,1)]
    it "a*x**2+b*x+c vs x**2 -2*x+1" $ do
      match (a*x**2+b*x+c) (x**2-2*x+1) `shouldBe` Just [(c,1),(b,-2),(a,1)]
  describe "polynomial solver" $ do
    it "3*x + 3 = 0" $ do
      solve (3*x+3) x `shouldBe` Just [-1]
    it "x**2 - 2*x + 1 = 0" $ do
      solve (x**2 - 2*x + 1) x `shouldBe` Just [1,1]
    it "x**2 - 4*x + 3 = 0" $ do
      solve (x**2 - 4*x + 3) x `shouldBe` Just [3,1]
  describe "linear solver" $ do
    it "x + y = 1,x - y = 3 " $ do
      lReductions [x+y-1,x-y-3] `shouldBe` [x+y-1,2*x-4]
    it "x = 0" $ do
      linsolve [x] `shouldBe` Just [(x,0)]
    it "x + y = 1,x - y = 3 == x = 2,y=-1" $ do
      linsolve [x+y=:1,x-y=:3] `shouldBe` Just [(x,2),(y,-1)]
    it "x+y+z=2,x+2*y+3*z=1,2*x+y+z=2" $ do
      linsolve [x+y+z=:2,x+2*y+3*z=:1,2*x+y+z=:2] `shouldBe` Just [(x,0),(y,5),(z,-3)]

    it "underdetermined system (2 equations, 3 variables) returns Nothing" $ do
      -- x + y + z = 1, x - y = 3 has infinitely many solutions for z
      linsolve [x+y+z=:1,x-y=:3] `shouldBe` Nothing

    it "CV variables are NOT treated as unknowns (correct behavior)" $ do
      -- CV represents constants in pattern matching, not unknowns
      let [a0,a1,a2] = map CV ["a0","a1","a2"]
      let eqs = [a1, 2*a2, a0 + a1 =: (-1)]
      linsolve eqs `shouldBe` Just []  -- No V variables, so empty solution

    it "V variables are treated as unknowns" $ do
      -- After converting CV to V, linsolve should work
      let [a0,a1,a2] = map V ["a0","a1","a2"]
      let eqs = [a1, 2*a2, a0 + a1 =: (-1)]
      linsolve eqs `shouldBe` Just [(a0,-1),(a1,0),(a2,0)]
