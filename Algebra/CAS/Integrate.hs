{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Integrate where

import Algebra.CAS.Base
import Algebra.CAS.Diff
import Data.List(nub)

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


-- | Find indeterminates of an expression
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> indets (x*y+z/x)
-- [x,y,z]
-- >>> indets (3*x^2-x*y-y^2)
-- [x,y]
-- >>> indets (sin(x)*cos(x)**2)
-- [sin(x),x,cos(x)]
indets :: Formula ->  [Formula]
indets = nub.indets'

indets' :: Formula ->  [Formula]
indets' (C _) = []
indets' (CV _) = []
indets' a@(V _) = [a]
indets' a@(S (Sin v)) = a:indets' v
indets' a@(S (Cos v)) = a:indets' v
indets' a@(S (Tan v)) = a:indets' v
indets' a@(S (Sinh v)) = a:indets' v
indets' a@(S (Cosh v)) = a:indets' v
indets' a@(S (Tanh v)) = a:indets' v
indets' a@(S (Asin v)) = a:indets' v
indets' a@(S (Acos v)) = a:indets' v
indets' a@(S (Atan v)) = a:indets' v
indets' a@(S (Asinh v)) = a:indets' v
indets' a@(S (Acosh v)) = a:indets' v
indets' a@(S (Atanh v)) = a:indets' v
indets' a@(S (Exp v)) = a:indets' v
indets' a@(S (Log v)) = a:indets' v
indets' a@(S (Abs v)) = a:indets' v
indets' a@(S (Sig v)) = a:indets' v
indets' a@(S (LogBase v0 v1)) = a:indets' v0 ++  a:indets' v1
indets' Pi = []
indets' I = []
indets' a@(S (Sqrt v)) = a:indets' v
indets' a@(S (Diff v0 v1)) = (a:indets' v0) ++  indets' v1
indets' a@(S (Integrate v0 v1)) = (a:indets' v0) ++  indets' v1
indets' (v0 :^: v1) = indets' v0 ++  indets' v1
indets' (v0 :*: v1) = indets' v0 ++  indets' v1
indets' (v0 :+: v1) = indets' v0 ++  indets' v1
indets' (v0 :/: v1) = indets' v0 ++  indets' v1

--genIndets' :: [Formula] -> 

terms :: Formula -> Formula -> [Formula]
terms f v = (indets' f) ++ map (\f -> diff f v) (indets f)

--derivativeTerms :: Formula -> Formula -> [Formula]
--derivativeTerms f v = 
{-
rishNorman :: Formula -> Formula -> Formula
rishNorman f x = error "not implemented"
  where
    si' = terms f x
    si = si' ++ map (\f -> diff f v) si'
-}
