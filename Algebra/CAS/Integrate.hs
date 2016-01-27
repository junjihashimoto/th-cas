{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Integrate where

import Algebra.CAS.Base
import Algebra.CAS.Diff

-- | integrate function
-- >>> let x = V "x"
-- >>> integrate x x
-- (1/2)*(x^2)
-- >>> integrate (x^2) x
-- (1/3)*(x^3)
-- >>> integrate (sin x) x
-- (-1)*(cos(x))
integrate :: Formula -> Formula -> Formula
integrate (x :+: y) z = (integrate x z) + (integrate y z)
integrate (a@(C (CI _)) :*: y) z = a * integrate y z
integrate (a@(C _) :*: y) z = a * integrate y z
integrate (C (CI a)) z = (C (CI a)) * z
integrate (C a) z = (C a) * z
integrate (S (Sin x')) y' | x' == y' = -1 * (S $ Cos x')
                          | otherwise = error "can not parse"
integrate (S (Cos x')) y' | x' == y' = (S (Sin x'))
                          | otherwise = error "can not parse"
integrate (x :^: (C (CI 2))) y | x == y    = x ** 3 / 3
                               | otherwise = error "can not parse"
integrate (x :^: (C (CI n))) y | x == y    = (x :^: (C (CI (n+1)))) / (fromIntegral (n+1))
                               | otherwise = error "can not parse"
integrate (V x) (V y) | x == y     = (V x) ** 2 / 2
                      | otherwise = error "can not parse"

integrate a b = error $ "can not parse : " ++ show a ++ " ##  " ++ show b

indets :: Formula -> [Formula]
indets _ = []

--genVariables :: [Formula] -> 

terms :: Formula -> Formula -> [Formula]
terms f v = (indets f) ++ map (\f -> diff f v) (indets f)

--derivativeTerms :: Formula -> Formula -> [Formula]
--derivativeTerms f v = 
{-
rishNorman :: Formula -> Formula -> Formula
rishNorman f x = error "not implemented"
  where
    si' = terms f x
    si = si' ++ map (\f -> diff f v) si'
-}
