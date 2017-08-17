
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Integrate where

import Algebra.CAS.Base
import Algebra.CAS.Diff
import Data.List(nub,sort)

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



-- | get terms of formula
-- 
-- >>> let [x,y] = map V ["x","y"]
-- >>> terms x x
-- [x]
-- >>> terms y x
-- []
-- >>> terms (sin x) x
-- [x,sin(x),cos(x)]
-- >>> terms (1/(sin x)) x
-- [x,sin(x),cos(x)]
terms :: Formula -> Formula -> [Formula]
terms f v = nub $ sort $ filter (\f' -> diff f' v /= 0) $ (indets f) ++ map (\f' -> diff f' v) (indets f)


-- | combination for candidate formula
-- 
-- >>> genPow 1 3
-- [[0,0,0],[0,0,1],[0,1,0],[1,0,0]]
-- >>> genPow 2 3
-- [[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,2,0],[1,0,0],[1,0,1],[1,1,0],[2,0,0]]
genPow :: Int -- ^ max degree of formula
       -> Int -- ^ length of terms
       -> [[Int]]
genPow  n w | n <= 0 = [map (\_ -> 0) [1..w]]
            | w <= 1 = map (\f -> [f]) [0..n]
            | otherwise = do
  a <- [0..n]
  b <- genPow (n-a) (w-1)
  return $ a:b


-- | degree for candidate formula
-- 
-- >>> let [x,y] = map V ["x","y"]
-- >>> candidateDegree 3 x
-- 1
-- >>> candidateDegree (sin x) x
-- 2
-- >>> candidateDegree (x**2) x
-- 3
candidateDegree :: Formula -- ^ formula
                -> Formula -- ^ variable
                -> Int -- ^ degree
candidateDegree  f x = 1 + (fromIntegral $ max (degree f) (degree (diff f x)))



-- | candidate formula
-- 
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> candidateFormula [x,y,z] 2
-- a0 + a6*x + a9*(x^2) + a3*y + a8*x*y + a5*(y^2) + a1*z + a7*x*z + a4*y*z + a2*(z^2)
candidateFormula :: [Formula] -- ^ variables
                 -> Int -- ^ degree
                 -> Formula -- ^ candidate formula
candidateFormula vars d =
  sum $ flip map (zip coeff pow) $ \(a,p) -> a * (foldr (*) 1 $ map (\(t,n) -> t**(fromIntegral n)) $ zip vars p)
  where
    coeff :: [Formula]
    coeff = reverse $ genCoeff "a" $ length pow
    pow = genPow d (length vars)

-- | derivation of candidate formula
-- 
-- >>> let [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9] = reverse $ genCoeff "a" 10
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> let candidate = a0 + a6*x + a9*(x^2) + a3*y + a8*x*y + a5*(y^2) + a1*z + a7*x*z + a4*y*z + a2*(z^2)
-- >>> derivationCandidate [x,y,z] [1,z,-y] candidate
-- a6 + 2*a9*x + a8*y + a7*z + (-1)*y*(a1 + a7*x + a4*y + 2*a2*z) + z*(a3 + a8*x + 2*a5*y + a4*z)
-- >>> expand $ (derivationCandidate [x,y,z] [1,z,-y] candidate ) - y
-- a6 + 2*a9*x + (-1)*y + (-1)*a1*y + a8*y + (-1)*a7*x*y + (-1)*a4*(y^2) + a3*z + a7*z + a8*x*z + (-2)*a2*y*z + 2*a5*y*z + a4*(z^2)
derivationCandidate :: [Formula] -- ^ variables
                    -> [Formula] -- ^ diff of variables
                    -> Formula -- ^ candidate formula
                    -> Formula -- ^ derivation of candidate formula
derivationCandidate vars dvars candidate =
  sum $ do
    (dv,v) <- zip dvars vars
    return $ dv * (diff candidate v)


-- | split formula by coeff and vars
-- 
-- >>> let [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9] = reverse $ genCoeff "a" 10
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> let f = a6 + 2*a9*x + (-1)*y + (-1)*a1*y + a8*y + (-1)*a7*x*y + (-1)*a4*(y^2) + a3*z + a7*z + a8*x*z + (-2)*a2*y*z + 2*a5*y*z + a4*(z^2)
-- >>> splitCoeffAndVariable f
-- [(a6,1),(2*a9,x),(-1 + (-1)*a1 + a8,y),((-1)*a7,x*y),((-1)*a4,y^2),(a3 + a7,z),(a8,x*z),((-2)*a2 + 2*a5,y*z),(a4,z^2)]
splitCoeffAndVariable :: Formula -> [(Formula,Formula)]
splitCoeffAndVariable formula = merge prelist
  where
    prelist = reverse $ map headV $ splitAdd formula
    merge :: [(Formula,Formula)] -> [(Formula,Formula)]
    merge [] = []
    merge ((c0,v0):[]) = [(c0,v0)]
    merge ((c0,v0):(c1,v1):xs) | v0 == v1 = merge ((c0+c1,v0):xs)
                               | otherwise = (c0,v0):(merge ((c1,v1):xs))

coeffToVariable :: Formula -> Formula
coeffToVariable formula = mapFormula c2v formula
  where
    c2v f =
      case f of
        (CV a) -> V a
        a -> a
                      
splitFactor :: Formula -> Formula -> (Formula, Formula)
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
rischNorman' :: Formula -> Formula -> Formula
rischNorman' f x = candidate
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



--heurischCandidate :: Formula -> Formula -> Formula
--heurischCandidate f x = 


--heurisch :: Formula -> Formula -> Formula
--heurisch f x = 
