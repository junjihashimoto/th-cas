{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.GrobnerBasis where

import Algebra.CAS.Base

sPolynomial :: Formula -> Formula -> Formula
sPolynomial f g = expand $ divAll (expand $ (ca*cb)*(lcmMonomial va vb)*f) (headAdd f)
                         - divAll (expand $ (ca*cb)*(lcmMonomial va vb)*g) (headAdd g)
  where
    (ca,va) = headV f
    (cb,vb) = headV g

allPair :: [t] -> [(t,t)]
allPair [] = []
allPair (x:xs) = map (\x' -> (x,x')) xs ++ allPair xs
  
grobnerG :: [Formula] -> [Formula]
grobnerG formulas = filter ((/=) 0) $ map (uncurry sPolynomial) $ allPair formulas

grobnerBasis :: [Formula] -> [Formula]
grobnerBasis formulas = map lc1 $ grobnerBasis' formulas $ allPair formulas
  where
    lc1 :: Formula -> Formula
    lc1 formula = expand $ formula / ca
      where
        (ca,_) = headV formula

grobnerBasis' :: [Formula] -> [(Formula,Formula)] -> [Formula]
grobnerBasis' formulas [] = formulas
grobnerBasis' formulas ((a,b):other) =
  case reductions (sPolynomial a b) formulas of
  0 -> grobnerBasis' formulas other
  c -> grobnerBasis (formulas++[c])

grobnerBasisIO :: [Formula] -> IO [Formula]
grobnerBasisIO formulas = grobnerBasisIO' formulas $ allPair formulas

grobnerBasisIO' :: [Formula] -> [(Formula,Formula)] -> IO [Formula]
grobnerBasisIO' formulas [] = return formulas
grobnerBasisIO' formulas aa@((a,b):other) = do
  print "formulas"
  print formulas
  print "div"
  print aa
  print "a"
  print a
  print "b"
  print b
  print "sPolynomial"
  print (sPolynomial a b)
  print "r"
  print (reductions (sPolynomial a b) formulas)
  case reductions (sPolynomial a b) formulas of
    0 -> grobnerBasisIO' formulas other
    c -> grobnerBasisIO (formulas++[c])
