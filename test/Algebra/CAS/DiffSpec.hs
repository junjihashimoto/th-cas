{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.CAS.DiffSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Algebra.CAS.Type
import Algebra.CAS.Algorithm.Diff

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
  describe "diff" $ do
    it "diff(x+y,x)" $ do
      diff (x+y) (x) `shouldBe` 1
    it "diff(x*x+y,x)" $ do
      diff (x*x+y) (x) `shouldBe` 2*x
    it "diff(sin(x*x)+y+z,x)" $ do
      diff (sin(x*x)+y+z) (x) `shouldBe` 2*cos(x*x)*x
    it "diff(log(x),x)" $ do
      diff (log(x)) (x) `shouldBe` 1/x
