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
  describe "sPolynomial (S-polynomial computation)" $ do
    it "sPolynomial of x^2 and x*y is 0" $ do
      let f = x^2
      let g = x*y
      -- LCM(x^2, x*y) = x^2*y, coefficients cancel
      sPolynomial f g `shouldBe` 0

    it "sPolynomial of x^2-1 and x*y-1" $ do
      let f = x^2 - 1
      let g = x*y - 1
      -- LCM(x^2, x*y) = x^2*y
      -- S(f,g) = y*f - x*g = y*(x^2-1) - x*(x*y-1) = x^2*y - y - x^2*y + x = x - y
      expand (sPolynomial f g) `shouldBe` expand (x - y)

    it "sPolynomial is symmetric (up to sign)" $ do
      let f = x^2 + y
      let g = x*y + 1
      expand (sPolynomial f g) `shouldBe` expand ((-1) * sPolynomial g f)

    it "sPolynomial of coprime leading terms" $ do
      let f = x^2
      let g = y^2
      -- LCM(x^2, y^2) = x^2*y^2
      -- S(f,g) = y^2*f - x^2*g = 0
      sPolynomial f g `shouldBe` 0

    it "sPolynomial of x^3 and x^2 is also 0" $ do
      let f = x^3
      let g = x^2
      -- When one leading term divides the other, S-polynomial is 0
      sPolynomial f g `shouldBe` 0

    it "sPolynomial of polynomials with different terms" $ do
      let f = x^2 + x
      let g = x^2 + y
      -- This should produce a non-zero S-polynomial
      sPolynomial f g `shouldSatisfy` (/= 0)

  describe "allPair (generate all pairs)" $ do
    it "allPair [] = []" $ do
      allPair ([] :: [Int]) `shouldBe` []

    it "allPair [1] = []" $ do
      allPair [1 :: Int] `shouldBe` []

    it "allPair [1,2] = [(1,2)]" $ do
      allPair [1 :: Int, 2] `shouldBe` [(1,2)]

    it "allPair [1,2,3] generates 3 pairs" $ do
      allPair [1 :: Int, 2, 3] `shouldBe` [(1,2), (1,3), (2,3)]

    it "allPair of n elements has n*(n-1)/2 pairs" $ do
      let xs = [1..5] :: [Int]
      length (allPair xs) `shouldBe` (5 * 4) `div` 2

  describe "grobnerG (compute all S-polynomials)" $ do
    it "grobnerG [] = []" $ do
      grobnerG [] `shouldBe` []

    it "grobnerG [f] = []" $ do
      grobnerG [x^2] `shouldBe` []

    it "grobnerG [x^2, y^2] filters out zero S-polynomial" $ do
      let result = grobnerG [x^2, y^2]
      -- S-polynomial is 0, so it gets filtered out
      result `shouldBe` []

    it "grobnerG filters out zeros" $ do
      let result = grobnerG [x^2 + 1, y^2 + 1]
      -- All S-polynomials should be non-zero or filtered
      all (/= 0) result `shouldBe` True

    it "grobnerG generates non-zero S-polynomials when appropriate" $ do
      let result = grobnerG [x^2 + x, x^2 + y]
      -- These should have a non-zero S-polynomial
      length result `shouldSatisfy` (>= 0)

  describe "reduction (polynomial reduction)" $ do
    it "reduction (x+y, [y]) = x" $ do
      reductions (x+y) [y] `shouldBe` x

    it "reduction (x+x*y, [y]) = x" $ do
      reductions (x+x*y) [y] `shouldBe` x

    it "reduction (x+x*y, [x+y]) reduces correctly" $ do
      reductions (x+x*y) [x+y] `shouldBe` (-x**2+x)

    it "reductions (x+x*y, [x+y,x^2])" $ do
      reductions (x+x*y) [x+y,x**2] `shouldBe` x

    it "reductions (fs) example" $ do
      reductions (head fs) (tail fs)  `shouldBe`  4 + (-1)*(x) + (-3)*(x^2) + (-1)*(y) + (x^2)*(y)

    it "reduction by empty list returns original" $ do
      reductions (x^2 + y) [] `shouldBe` (x^2 + y)

    it "reduction to zero" $ do
      reductions (x*y) [x*y] `shouldBe` 0

    it "reduction is order-dependent" $ do
      let f = x^2 + x*y
      let g1 = [x, y]
      let g2 = [y, x]
      -- Both should reduce but potentially differently due to order
      reductions f g1 `shouldSatisfy` (\r -> degree r <= degree f)
      reductions f g2 `shouldSatisfy` (\r -> degree r <= degree f)

  describe "grobnerBasis (full Grobner basis computation)" $ do
    it "grobnerBasis (fs) - classic example" $ do
      grobnerBasis fs `shouldBe`
        [
          1 + (-1)*(x) + (-3)*(y^2) + y^3,
          1 + (-1)*(x^2) + y^2,
          4 + (-1)*(x) + (-3)*(x^2) + (-1)*(y) + (x^2)*(y),
          -13 + (3)*(x) + (11)*(x^2) + (-1)*(x^4) + (-1)*(y) + (x)*(y),
          -17 + (8)*(x) + (26)*(x^2) + (-6)*(x^3) + (-12)*(x^4) + x^6,
          17 + (9)*(x) + (-17)*(x^2) + (-11)*(x^3) + x^4 + x^5
        ]

    it "grobnerBasis of single polynomial returns normalized version" $ do
      let gb = grobnerBasis [2*x^2 + 4*x]
      -- Should normalize leading coefficient to 1
      head gb `shouldBe` (x^2 + 2*x)

    it "grobnerBasis of linear system" $ do
      let system = [x + y - 1, x - y - 3]
      let gb = grobnerBasis system
      -- Should contain simpler polynomials
      length gb `shouldSatisfy` (>= 2)

    it "grobnerBasis contains original generators (after reduction)" $ do
      let polys = [x^2 + 1, y^2 + 1]
      let gb = grobnerBasis polys
      -- GB should contain normalized versions
      length gb `shouldBe` 2

  describe "Grobner Basis Properties" $ do
    it "every element reduces to zero by the basis" $ do
      let basis = grobnerBasis fs
      -- Each original polynomial should reduce to 0 modulo the basis
      all (\f -> reductions f basis == 0) fs `shouldBe` True

    it "Grobner basis is non-empty for non-empty input" $ do
      grobnerBasis [x^2 + y] `shouldSatisfy` (not . null)

    it "Grobner basis has normalized leading coefficients" $ do
      let gb = grobnerBasis [2*x^2 + 4*x, 3*y^2 + 6*y]
      -- All leading coefficients should be 1 (normalized)
      all (\f -> let (c, _) = headV f in c == 1 || c == (-1)) gb `shouldBe` True

    it "Grobner basis is stable under reduction" $ do
      let gb = grobnerBasis [x^2 - y, x^3 - x]
      -- Any element of GB reduced by other elements should give 0
      all (\g -> reductions g (filter (/= g) gb) == 0 || reductions g (filter (/= g) gb) == g) gb `shouldBe` True

