
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Integrate where

import Algebra.CAS.Base
import Algebra.CAS.Diff
import Data.List(nub)

-- | integrate function
-- 
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



--genIndets' :: [Formula] -> 

terms :: Formula -> Formula -> [Formula]
terms f v = (indets f) ++ map (\f' -> diff f' v) (indets f)

--derivativeTerms :: Formula -> Formula -> [Formula]
--derivativeTerms f v = 

genPow :: Int -> Int -> [[Int]]
genPow  n w | n <= 0 = [map (\f -> 0) [1..w]]
            | w <= 1 = map (\f -> [f]) [0..n]
            | otherwise = do
  a <- [0..n]
  b <- genPow (n-a) (w-1)
  return $ a:b

splitFactor p x' =
  case (filter (\t -> isVariable (diff t x')) $ indets p) of
  [] -> (1,p)
  x:_ ->
    let (c,q) = content p x
        (spl1,spl2) = splitFactor c x'
        s = gcdPolynomial q (diff q x') `quot` gcdPolynomial q (diff q x)
        (splh1,splh2) = splitFactor (q `quot` s)  x'
    in if degree s == 0
       then (spl1,q*spl2)
       else (spl1*splh1*s,spl2*splh2)
      

-- | integrate function of rischNorman-algorithm
-- This is under development.
rischNorman :: Formula -> Formula -> Formula
rischNorman f x = candidate
  where
    ids = nub $ indets f ++ indets (diff f x)
    ids_diff = map (flip diff x) ids
    len = length ids
    d   = fromIntegral $ max (degree f) (degree (diff f x))
    pow :: [[Int]]
    pow = genPow d len
    vars :: [Formula]
    vars = genCoeff "a" $ length pow
    candidate = foldr (+) 0 $ flip map (zip vars pow) $ \(a,p) -> a * (foldr (*) 1 $ map (\(t,n) -> t**(fromIntegral n)) $ zip ids p)


