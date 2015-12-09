{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE CPP#-}

module Algebra.CAS.Type where

import Language.Haskell.TH
import Data.String

data Const =
   CZero
 | COne
 | CI Integer
 | CR Rational
 | CFI Integer Integer
 | Pi
 deriving (Show,Eq,Ord)


-- | Mathematical expression
data Value =
   Zero
 | One
 | C Const
 | V Name
 | Sin Value
 | Cos Value
 | Tan Value
 | Sinh Value
 | Cosh Value
 | Tanh Value
 | Asin Value
 | Acos Value
 | Atan Value
 | Asinh Value
 | Acosh Value
 | Atanh Value
 | Exp Value
 | Log Value
 | Abs Value
 | Sig Value
 | LogBase Value Value
 | Sqrt Value
 | Diff Value Value
 | Other Exp
 | Value :^: Value
 | Value :*: Value
 | Value :+: Value
 | Value :-: Value
 | Value :/: Value
 | DivideByZero
 deriving (Show,Eq,Ord)

data ListValue =
   Sum [ListValue] |
   Mul [ListValue] |
   Fraction {
     numer :: ListValue
   , denom :: ListValue
   } |
   CoeffAndValue {
     consts :: [Value]
   , variables :: [Value]
   } deriving (Show,Eq,Ord)
 
data Polynomial =
  Polynomial {
    coeffs :: [Value]
  , variable :: Maybe Value
  } deriving (Show,Eq,Ord)

neg = CI (-1)

#if MIN_VERSION_template_haskell(2,10,0)
#else
deriving instance Ord Exp
#endif

instance Num Value where
  fromInteger 0 = Zero
  fromInteger 1 = One
  fromInteger a = CI (fromIntegral a)
  (+) a b = 
    case (a,b) of
    (Zero,Zero) -> Zero
    (Zero,b) -> b
    (a,Zero) -> a
    (One,One) -> CI 2
    (One,CI b') | b' == -1 -> Zero
                | otherwise -> CI (b'+1)
    (CI a',One) | a' == -1 -> Zero
                | otherwise -> CI (a'+1)
    (One,C b') | b' == -1 -> Zero
               | otherwise -> C (b'+1)
    (C a',One) | a' == -1 -> Zero
               | otherwise -> C (a'+1)
    (CI a',CI b') | a'+b' == 0 -> Zero
                  | a'+b' == 1 -> One
                  | otherwise -> CI (a'+b')
    (C a',C b') | a'+b' == 0 -> Zero
                | a'+b' == 1 -> One
                | otherwise -> C (a'+b')
    ((a' :+: (V b')),c') -> (a' + c') :+: V b'
    (a',b') | compare a' b' == LT  -> a' :+: b'
            | compare a' b' == GT  -> b' :+: a'
            | otherwise -> a' :+: b'
  (-) a b | a == b = Zero
          | otherwise = a + (neg * b)
  (*) a b =
    case (a,b) of
    (Zero,_) -> Zero
    (_,Zero) -> Zero
    (One,b) -> b
    (a,One) -> a
    (CI a',CI b') -> CI (a'*b')
    (C a',C b') -> C (a'*b')
    (V a',V b') | a' == b' -> V a' ** 2
                | otherwise -> V a' :*: V b'
    ((a' :*: (V b')),c') -> (a' * c') :*: V b'
    ((a' :/: b'),c') -> (a' * c') / b'
    (a',(b' :/: c')) -> (a' * b') / c'
    ((a' :/: b'),(c' :/: d')) -> (a' * b') / (c' * d')
    (a',b') | compare a' b' == LT  -> a' :*: b'
            | compare a' b' == GT  -> b' :*: a'
            | otherwise -> a' :*: b'
  abs a = Abs a
  signum a = Sig a

instance Fractional Value where
  fromRational 0 = Zero
  fromRational 1 = One
  fromRational a = C (fromRational a)
  recip a = (/) One a
  (/) a b =
    case (a,b) of
    (Zero,_) -> Zero
    (_,Zero) -> DivideByZero
    (One,One) -> One
    (One,b) -> One :/: b
    (a,One) -> a
    (CI a',CI b') -> case rem a' b' of
       0 -> CI (a'`div`b')
       otherwise -> CI a' :/: CI b'
    (C a',C b') -> C (a'/b')
    (a',b') | a' == b' -> One
            | otherwise -> a :/: b

instance Floating Value where
  pi = Pi
  exp Zero = One
  exp a = Exp a
  sqrt a = Sqrt a
  log One = Zero
  log a = Log a
  (**) a Zero = One
  (**) a One = a
  (**) a b = (:^:) a b
  logBase = LogBase
  sin = Sin
  tan = Tan
  cos = Cos
  asin = Asin
  atan = Atan
  acos = Acos
  sinh = Sinh
  tanh = Tanh
  cosh = Cosh
  asinh = Asinh
  atanh = Atanh
  acosh = Acosh

instance IsString Value where
  fromString = val

-- | Lift String to variable of Value
val ::  String ->  Value
val v = V (mkName v)

degree :: Polynomial -> Int
degree poly = length (coeffs poly)

listPlus :: [Value] -> [Value] -> [Value]
listPlus (x:xs) (y:ys) = (x+y):listPlus xs ys
listPlus (x:xs) [] = x:listPlus xs []
listPlus [] (y:ys) = y:listPlus [] ys
listPlus [] [] = []

listMul :: [Value] -> [Value] -> [Value]
listMul xs (y:ys) = listPlus (map (* y) xs) (Zero:(listMul xs ys))
listMul xs [] = []

listDivRem :: [Value] -> [Value] -> ([Value],[Value])
listDivRem xs ys | length xs < length ys = ([Zero],xs)
listDivRem xs ys | otherwise =
  let dx = length xs - 1
      dy = length ys - 1
      cx = xs !! dx
      cy = ys !! dy
      divnum = cx / cy
      rem' = take dx $ listPlus xs ((replicate (dx-dy) Zero) ++ (map (\v -> -v * divnum) ys))
      (div'',rem'') = listDivRem rem' ys
  in (listPlus div'' ((replicate (dx-dy) Zero) ++ [divnum]) ,rem'')

polynomialOperator :: ([Value] -> [Value] -> [Value]) -> Polynomial -> Polynomial -> Polynomial
polynomialOperator op (Polynomial a0 (Just v0)) (Polynomial a1 (Just v1)) | v0 == v1 = Polynomial (op a0 a1) (Just v0)
                                                                          | otherwise = error "variables of polynomial are differentl"
polynomialOperator op (Polynomial a0 (Just v0)) (Polynomial [a1] Nothing) = Polynomial (op a0 [a1]) (Just v0)
polynomialOperator op (Polynomial [a0] Nothing) (Polynomial a1 (Just v1)) = Polynomial (op [a0] a1) (Just v1)
polynomialOperator op (Polynomial [a0] Nothing) (Polynomial [a1] Nothing) = Polynomial (op [a0] [a1]) Nothing
polynomialOperator op _ _ = error "constant polynomial is not constant"

instance Num Polynomial where
  fromInteger 0 = Polynomial [Zero] Nothing
  fromInteger 1 = Polynomial [One] Nothing
  fromInteger a = Polynomial [CI (fromIntegral a)]  Nothing
  (+) p0 p1 = polynomialOperator listPlus p0 p1
  (-) a (Polynomial a1 v1) = (+) a (Polynomial (map (* neg) a1) v1)
  (*) p0 p1 = polynomialOperator listMul p0 p1
  abs a = error "not defined abs for polynomial"
  signum a = error "not defined signum for polynomial"

instance Enum Polynomial where
  succ (Polynomial (a:ax) v) = Polynomial ((a+1):ax) v
  succ (Polynomial [] v) = Polynomial [1] v
  pred (Polynomial (a:ax) v) = Polynomial ((a-1):ax) v
  pred (Polynomial [] v) = Polynomial [-1] v
  toEnum v = Polynomial [CI (fromIntegral v)] Nothing

instance Real Polynomial where
  toRational (Polynomial (CI v:_) _) = toRational v
  toRational (Polynomial (C v:_) _) = toRational v
  toRational (Polynomial (Pi:_) _) = toRational pi
  toRational (Polynomial (Zero:_) _) = toRational pi
  toRational (Polynomial (One:_) _) = toRational 1
  toRational (Polynomial _ _) = toRational 0

instance Integral Polynomial where
  quot a b = polynomialOperator (\a b -> fst $ listDivRem a b) a b
  rem a b = polynomialOperator (\a b -> snd $ listDivRem a b) a b
  div = quot
  mod = rem

isZero (Polynomial [] _ ) = True
isZero (Polynomial [Zero] _ ) = True
isZero (Polynomial _ _ ) = False

polynomialGcd :: Polynomial -> Polynomial -> Polynomial
polynomialGcd p0 p1 | isZero p0 = p1
                    | isZero p1 = p0
                    | degree p0 > degree p1 = polynomialGcd (p0 `rem` p1) p1
                    | otherwise             = polynomialGcd p0 (p1 `rem` p0)
{-
fromValue :: Value -- ^ formula
          -> Value -- ^ variable
          -> Polynomial
fromValue formula variable =
-}

fromPolynomial :: Polynomial -> Value
fromPolynomial (Polynomial [] _) = Zero
fromPolynomial (Polynomial [p0] Nothing) = p0
fromPolynomial (Polynomial p0 (Just v)) = foldr func Zero $ zip [0..] p0
  where
     func :: (Integer,Value)->Value->Value
     func (a,b) c = b*(v ** (fromInteger a))+c

--partialFractionExpansion =
{-
toListValue :: Value -> ListValue
toListValue (a:*:b) = Mul $ toList a ++ toList b
  where
    toList :: Value -> [ListValue]
    toList a = 
      case toListValue a of
      Mul a' -> a'
      Mul a' -> error ""
toListValue (a:+:b) = Sum $ toList a ++ toList b
  where
    toList :: Value -> [ListValue]
    toList a = 
      case toListValue a of
      Sum a' -> a'
      Mul a' -> error ""
-}
