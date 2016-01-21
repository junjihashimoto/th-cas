{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.CAS.SolveSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Algebra.CAS.Type
import Algebra.CAS.Algorithm.Solve

main :: IO ()
main = hspec spec

x :: Value
x = "x"
y :: Value
y = "y"
z :: Value
z = "z"

a :: Value
a = CV "a"
b :: Value
b = CV "b"
c :: Value
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
    it "x + y = 1,x - y = 3 == x = 2,y=-1" $ do
      linsolve [x+y-1,x-y-3] `shouldBe` Just [(x,2),(y,-1)]
