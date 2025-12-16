{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Integrate where

import Algebra.CAS.Base
import Algebra.CAS.Diff
import Algebra.CAS.Solve (linsolve)
import Data.List (nub, sort)

-- | Main integration entry point.
-- >>> let x = V "x"
-- >>> integrate (x^2) x
-- (1/3)*(x^3)
integrate :: Formula -> Formula -> Formula
integrate f v = integrateFormula f v
  where
    integrateFormula :: Formula -> Formula -> Formula
    -- Sum rule: ∫(f + g) = ∫f + ∫g
    integrateFormula (f1 :+: f2) var = integrateFormula f1 var + integrateFormula f2 var

    -- Constant multiple rule: ∫(c*f) = c*∫f
    integrateFormula (c :*: f') var
      | isConst c = c * integrateFormula f' var
    integrateFormula (f' :*: c) var
      | isConst c = c * integrateFormula f' var

    -- Constant: ∫c dx = c*x
    integrateFormula c@(C _) var = c * var

    -- Variable: ∫x dx = x²/2
    integrateFormula f' var
      | f' == var = (var ** 2) / 2

    -- Power rule: ∫x^n dx = x^(n+1)/(n+1) for n ≠ -1
    integrateFormula (var' :^: (C (CI n))) var
      | var' == var && n /= -1 = (var ** (fromIntegral (n+1))) / (fromIntegral (n+1))
      | var' == var && n == -1 = S (Log var)  -- ∫x^(-1) dx = ln(x)

    -- Trigonometric functions
    integrateFormula (S (Sin f')) var
      | f' == var = (-1) * S (Cos var)
    integrateFormula (S (Cos f')) var
      | f' == var = S (Sin var)

    -- Division: check for 1/x or rational functions
    integrateFormula f'@(_ :/: _) var =
      let n = numer f'
          d = denom f'
      in if n == 1 && d == var
           then S (Log var)  -- ∫1/x dx = ln(x)
           else if isRational f' var
                  then integrateRational f' var
                  else error $ "integrate: cannot integrate " ++ show f'

    -- Fallback: try Risch-Norman heuristic
    integrateFormula f' var = rischNorman' f' var

-- | Check if formula is a rational function in x (contains only arithmetic and powers of x)
isRational :: Formula -> Formula -> Bool
isRational f x = all (\v -> v == x || isConst v) (indets f)

-- | Integrate a rational function P/Q using Hermite Reduction.
-- \int \frac{P}{Q} dx = \frac{C}{D} + \int \frac{A}{B} dx
-- where B is square-free.
integrateRational :: Formula -> Formula -> Formula
integrateRational f x =
  let n = numer f
      d = denom f
      -- 1. Perform Hermite Reduction to separate Rational and Logarithmic parts
      (ratPart, logIntegrand) = hermiteReduction n d x
  in expand $ ratPart + integrateLogPart logIntegrand x

-- | Hermite Reduction
-- Decomposes \int (P/Q) into (RationalPart) + \int (Remainder/SquareFreeQ)
hermiteReduction :: Formula -> Formula -> Formula -> (Formula, Formula)
hermiteReduction p q x =
  let -- 1. Square-free factorization: Q = q1 * q2^2 * ... * qk^k
      sqFactors = squareFree q x

      -- Helper to process factors from highest power to 1
      reduce :: [(Formula, Int)] -> (Formula, Formula)
      reduce [] = (0, 0)
      reduce ((v, 1):vs) =
        let (r, l) = reduce vs
        in (r, expand $ l + (v * product [fac^exp | (fac, exp) <- vs])) -- Accumulate denominator for log part

      reduce ((v, k):vs) | k > 1 =
        -- Reduction formula for P/V^k
        -- We want to solve P = A*V' + B*V (Extended Euclidean) to reduce power
        -- But Hermite is specific:
        -- Int(A/V^k) = -B/((k-1)V^(k-1)) + Int(...)

        -- Simplified recursive step:
        -- We calculate the partial fraction for this power V^k
        -- (This is a simplified view; full Hermite does this globally, but we do iterative reduction here)
        let (rAcc, lAcc) = reduce vs
        in (rAcc, lAcc) -- Placeholder: requires robust polynomial division to be fully effective

      reduce _ = (0, p/q)

      -- Fallback to step-based reduction if not factored perfectly
      fallbackStep = hermiteStep p q x

  in if null sqFactors
       then (0, p/q)
       else fallbackStep

-- | Single step Hermite Reduction (Mack's Algorithm variant)
-- Returns (RationalPart, RemainingIntegrand)
hermiteStep :: Formula -> Formula -> Formula -> (Formula, Formula)
hermiteStep a d x =
  let (g, u, v) = extendedEuclidean d (diff d x) x
      -- g = gcd(d, d'), d = g * d_star
      -- If degree(g) == 0, d is square-free.
  in if degree g == 0
       then (0, a / d)
       else
         -- Basic approximation for Hermite step
         -- If we have 1/x^2, g=x.
         -- We should ideally solve the system to reduce the power.
         -- Since full polynomial division is pending, we defer complex rational parts to Heuristic
         -- except for obvious cases like 1/x^n handled by integration rules in Diff/Base.
         (0, a/d)

-- | Integrate the Logarithmic Part \int (A/D_sf) dx
-- D_sf is square-free.
-- Current strategy:
-- 1. If degree(D_sf) == 1 (linear), result is A * log(D_sf)
-- 2. If degree(D_sf) == 2 (quadratic), complete square -> atan or log
-- 3. Else fallback to Risch-Norman heuristic.
integrateLogPart :: Formula -> Formula -> Formula
integrateLogPart f x = rischNorman' f x

-- | Square-Free Factorization of a polynomial P(x)
-- Returns list of (Factor, Multiplicity)
-- P = \prod P_i ^ i
squareFree :: Formula -> Formula -> [(Formula, Int)]
squareFree p x =
  let p' = diff p x
      c  = gcdPolynomial p p'
      w  = expand $ p / c
      y  = expand $ p' / c
      z  = expand $ diff w x

      -- Simplified square free logic for now:

  in if degree p <= 0
       then []
       else if degree c == 0
              then [(p, 1)]
              else [(p, 1)] -- Placeholder for full Yun's algorithm

-- | Extended Euclidean Algorithm for Polynomials
-- Returns (gcd, s, t) such that s*a + t*b = gcd(a,b)
extendedEuclidean :: Formula -> Formula -> Formula -> (Formula, Formula, Formula)
extendedEuclidean a b x
  | degree b == 0 && b == 0 = (a, 1, 0)
  | otherwise =
      let (q, r) = quotRemPoly a b x
          (g, s, t) = extendedEuclidean b r x
      in (g, t, expand $ s - (q * t))

-- | Polynomial Quotient and Remainder
-- Wraps the Base implementation but ensures we treat them as polynomials in x
quotRemPoly :: Formula -> Formula -> Formula -> (Formula, Formula)
quotRemPoly a b x = reduction a b -- Reuse existing reduction which is multivariate division

-- | Risch-Norman Heuristic
-- This implementation generates a candidate polynomial based on the variables
-- and derivatives found in the integrand, differentiates it, and solves the
-- resulting linear system to find the coefficients.
rischNorman' :: Formula -> Formula -> Formula
rischNorman' f x =
  case solvedCoeffs of
    Just sol -> simplifyResult $ subst (convertSolution sol) candidate
    Nothing  -> error $ "integrate // Risch-Norman heuristic failed for: " ++ show f
  where
    -- 1. Identify basis kernels (indeterminates + their derivatives)
    ids = nub $ (x : indets f) ++ indets (diff f x)

    -- 2. Determine degree for the Ansatz
    d = candidateDegree f x

    -- 3. Generate Candidate (Ansatz)
    -- candidate = a0 + a1*x + ... + an*vars^d
    candidate = candidateFormula ids d

    -- 4. Differentiate the Candidate w.r.t integration variable x
    dCandidate = expand $ diff candidate x

    -- 5. Form the linear system: dCandidate - f = 0
    equation = expand (dCandidate - f)

    -- 6. Extract coefficients of the basis terms from the equation.
    systemPoly = splitCoeffAndVariable equation
    linearSystem = map (coeffToVariable . fst) systemPoly

    -- 7. Solve the linear system
    solvedCoeffs = linsolve linearSystem

    -- Convert V variables from linsolve to CV variables for candidate
    convertSolution :: [(Formula, Formula)] -> [(Formula, Formula)]
    convertSolution = map (\(V name, val) -> (CV name, val))

    simplifyResult (C Zero) = C Zero
    simplifyResult res = expand res

-- === Helper Functions ===

terms :: Formula -> Formula -> [Formula]
terms f v = nub $ sort $ filter (\f' -> diff f' v /= 0) (indets f) ++ map (\f' -> diff f' v) (indets f)

genPow :: Int -> Int -> [[Int]]
genPow  n w | n <= 0 = [map (\_ -> 0) [1..w]]
            | w <= 1 = map (\f -> [f]) [0..n]
            | otherwise = do
                a <- [0..n]
                b <- genPow (n-a) (w-1)
                return $ a:b

candidateDegree :: Formula -> Formula -> Int
candidateDegree  f x = 1 + (fromIntegral $ max (degree f) (degree (diff f x)))

candidateFormula :: [Formula] -> Int -> Formula
candidateFormula vars d =
  sum $ flip map (zip coeff pow) $ \(a,p) -> a * (foldr (*) 1 $ map (\(t,n) -> t**(fromIntegral n)) $ zip vars p)
  where
    coeff = reverse $ genCoeff "a" $ length pow
    pow = genPow d (length vars)

derivationCandidate :: [Formula] -> [Formula] -> Formula -> Formula
derivationCandidate vars dvars candidate =
  sum $ do
    (dv,v) <- zip dvars vars
    return $ dv * (diff candidate v)

splitCoeffAndVariable :: Formula -> [(Formula,Formula)]
splitCoeffAndVariable formula = merge prelist
  where
    prelist = reverse $ map headV $ splitAdd formula
    merge [] = []
    merge ((c0,v0):[]) = [(c0,v0)]
    merge ((c0,v0):(c1,v1):xs) | v0 == v1 = merge ((c0+c1,v0):xs)
                               | otherwise = (c0,v0):(merge ((c1,v1):xs))

coeffToVariable :: Formula -> Formula
coeffToVariable formula = mapFormula c2v formula
  where
    c2v (CV a) = V a
    c2v a = a
