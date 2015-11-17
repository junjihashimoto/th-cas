{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -ddump-splices #-}

import Test.Hspec
import Algebra.CAS
import qualified Algebra.CAS.TH as TH
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

main :: IO ()
main = hspec $ do
  describe "th-diff" $ do
    it "diff(x+1,x)" $ do
      let x = error ""
      $(TH.diff [|x+1|] [|x|]) `shouldBe` 1
    it "diff(x^2+x+1,x)" $ do
      let x = 3
      $(TH.diff [|x*x+1+x|] [|x|]) `shouldBe` (2*x + 1)
    it "diff(diff(x^3+x+1,x)+x,x)" $ do
      let x = 3
      $(TH.diff [|$(TH.diff [|x*x*x+1+x|] [|x|]) + x|] [|x|])  `shouldBe` (6*x+1)
    it "diff(x+y*x,x)" $ do
      let x = 3
          y = 100
      $(TH.diff [|x+x*x*y|] [|x|])  `shouldBe` (2*x*y+1)
  describe "cas" $ do
    it "diff(x+y,x)" $ do
      let x = val "x"
          y = val "y"
      simp (diff (x+y) (x)) `shouldBe` 1
    it "diff(x*x+y,x)" $ do
      let x = val "x"
          y = val "y"
      simp (diff (x*x+y) (x)) `shouldBe` 2*x
    it "destructAdd" $ do
      let x = val "x"
          y = val "y"
      destructAdd (x*x+y+x*y*(3+x)) `shouldBe` [x*x,y,x*y*(3+x)]
    it "simp" $ do
      let x = val "x"
          y = val "y"
      simp (2*y+x+x+y) `shouldBe` 2*x+3*y
