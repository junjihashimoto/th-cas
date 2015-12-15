{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Algebra.CAS
import qualified Algebra.CAS.TH as TH

main :: IO ()
main = hspec $ do
  describe "th-diff" $ do
    prop "diff(x+1,x)" $ \(x :: Int) -> 
      $(TH.diff [|x+1|] [|x|]) ==  1
    prop "diff(x^2+x+1,x)" $ \(x :: Int) -> 
      $(TH.diff [|x*x+1+x|] [|x|]) == (2*x + 1)
    prop "diff(diff(x^3+x+1,x)+x,x)" $ \(x :: Int) -> 
      $(TH.diff [|$(TH.diff [|x*x*x+1+x|] [|x|]) + x|] [|x|]) == (6*x+1)
    prop "diff(x+y*x,x)" $ \((x,y) :: (Int,Int)) -> 
      $(TH.diff [|x+x*x*y|] [|x|]) == (2*x*y+1)
    prop "diff(1/sin(x),x)" $ \(x ::  Float) -> 
      $(TH.diff [|1/sin(x)|] [|x|]) ==  - cos(x)/sin(x)^2

  describe "cas" $ do
    it "diff(x+y,x)" $ do
      let x = "x" ::  Value
          y = "y" ::  Value
      (diff (x+y) (x)) `shouldBe` 1
    it "diff(x*x+y,x)" $ do
      let x = "x" ::  Value
          y = "y" ::  Value
      (diff (x*x+y) (x)) `shouldBe` 2*x
    it "simp" $ do
      let x = "x" ::  Value
          y = "y" ::  Value
      (2*y+x+x+y) `shouldBe` 2*x+3*y
