{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Type where

import Language.Haskell.TH

data Value =
   V Name
 | C Rational
 | CI Integer
 | Neg Value
 | Value :^: Value
 | Value :*: Value
 | Value :+: Value
 | Value :-: Value
 | Value :/: Value
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
 | Pi
 | Sqrt Value
 | Diff Value Value
 | Other Exp
 deriving (Show,Eq,Ord)

instance Num Value where
  fromInteger a = CI (fromIntegral a)
  (+) a b = a :+: b
  (-) a b = a :-: b
  (*) a b = a :*: b
  abs a = Abs a
  signum a = Sig a

instance Fractional Value where
  fromRational a = C (fromRational a)
  recip a = (C 1.0) :/: a
  (/) a b = a :/: b

instance Floating Value where
  pi = Pi
  exp = Exp
  sqrt = Sqrt
  log = Log
  (**) = (:^:)
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

val :: String -> Value
val v = V (mkName v)
