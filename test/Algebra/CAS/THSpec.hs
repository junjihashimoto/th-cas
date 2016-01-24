{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.CAS.THSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Algebra.CAS()
import qualified Algebra.CAS.TH as TH

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "th-diff" $ do
    prop "diff(x+1,x)" $ \(x :: Int) -> 
      $(TH.diff [|x+1|] [|x|]) ==  1
    prop "diff(x^2+x+1,x)" $ \(x :: Int) -> 
      $(TH.diff [|x*x+1+x|] [|x|]) == (2*x + 1)
    prop "diff(diff(x^3+x+1,x)+x,x)" $ \(x :: Int) -> 
      $(TH.diff [|$(TH.diff [|x*x*x+1+x|] [|x|]) + x|] [|x|]) == (6*x+1)
    prop "diff(x+y*x,x)" $ \((x,y) :: (Int,Int)) -> 
      $(TH.diff [|x+x*x*y|] [|x|]) == (2*x*y+1)
    prop "diff(sin(cos(x)),x)" $ \(x ::  Float) -> 
      $(TH.diff [|sin(cos(x))|] [|x|]) ==  - (sin x)*cos(cos x)
