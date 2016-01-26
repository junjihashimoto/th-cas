{-# LANGUAGE OverloadedStrings #-}

module Algebra.CAS.GrobnerBasisSpec (main, spec) where

import Test.Hspec
import Algebra.CAS.Base
import Algebra.CAS.GrobnerBasis

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

fs  = [  1 -x        - 3*y**2 + y**3,
        -1    + x**2 -   y**2 
      ]

spec :: Spec
spec = do
  describe "reduction" $ do
    it "reduction (x+y ,y)" $ do
      reductions (x+y) [y] `shouldBe` x
    it "reduction (x+x*y ,y)" $ do
      reductions (x+x*y) [y] `shouldBe` x
    it "reduction (x+x*y ,x+y)" $ do
      reductions (x+x*y) [x+y] `shouldBe` (-x**2+x)
    it "reductions (x+x*y ,[x+y,x])" $ do
      reductions (x+x*y) [x+y,x**2] `shouldBe` x
    it "reductions (fs)" $ do
      reductions (head fs) (tail fs)  `shouldBe`  4 + (-1)*(x) + (-3)*(x^2) + (-1)*(y) + (x^2)*(y)
  describe "grobnerBasis" $ do　-- under development
    it "grobnerBasis (fs)" $ do
      grobnerBasis fs `shouldBe`
        [
          1 + (-1)*(x) + (-3)*(y^2) + y^3,
          1 + (-1)*(x^2) + y^2,
          4 + (-1)*(x) + (-3)*(x^2) + (-1)*(y) + (x^2)*(y),
          -13 + (3)*(x) + (11)*(x^2) + (-1)*(x^4) + (-1)*(y) + (x)*(y),
          -17 + (8)*(x) + (26)*(x^2) + (-6)*(x^3) + (-12)*(x^4) + x^6,
          17 + (9)*(x) + (-17)*(x^2) + (-11)*(x^3) + x^4 + x^5
        ]
      
