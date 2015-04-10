{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module CAS where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import GHC.Num

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
 deriving (Show,Eq)

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


diff :: Value -> Value -> Value
diff (V x') (V y') | x' == y' = CI 1
                   | otherwise = CI 0
diff (x :+: y) z = (diff x z) + (diff y z)
diff (x :-: y) z = (diff x z) - (diff y z)
diff (x :*: y) z = (diff x z) * y + x * (diff y z)
diff (x :/: y) z = ((diff x z) * y - x * (diff y z)) / (y * y)
diff (x :^: CI 2) z = 2 * x * (diff x z)
diff (x :^: CI n) z = (fromIntegral n) * (x ** (fromIntegral (n-1))) * (diff x z)
diff (Sin x') y' = (Cos x') * (diff x' y')
diff (Cos x') y' = -1 * (Sin x') * (diff x' y')
diff (Exp x') y' = (Exp x') * (diff x' y')

diff (CI _) _ = CI 0
diff (C _) _ = CI 0
diff Pi _ = CI 0
diff (Log x') y' = recip x' * diff x' y'

diff a b = error $ "can not parse : " ++ show a ++ " ##  " ++ show b

diff' a b = simp $ diff a b

integrate :: Value -> Value -> Value
integrate (x :+: y) z = (integrate x z) + (integrate y z)
integrate (a@(CI _) :*: y) z = a * integrate y z
integrate (a@(C _) :*: y) z = a * integrate y z
integrate (CI a) z = (CI a) * z
integrate (C a) z = (C a) * z
--integrate (z :^: CI n) z = (x ** (fromIntegral (n-1))) * (integrate x z)
integrate (Sin x') y' | x' == y' = Cos x'
                      | otherwise = error "can not parse"
integrate (Cos x') y' | x' == y' = -1 * Sin x'
                      | otherwise = error "can not parse"
integrate (x :^: CI 2) y | x == y    = x ** 3 / 3
                         | otherwise = error "can not parse"
integrate (x :^: CI n) y | x == y    = (x :^: (CI (n+1))) / (fromIntegral (n+1))
                         | otherwise = error "can not parse"
integrate (x :^: CI n) z = (fromIntegral n) * (x ** (fromIntegral (n-1))) * (diff x z)

integrate a b = error $ "can not parse : " ++ show a ++ " ##  " ++ show b


simp :: Value -> Value
simp (C a :+: C b) = C (a+b)
simp (CI a :+: CI b) = CI (a+b)
simp (x :+: C 0) = simp x
simp (x :+: CI 0) = simp x
simp (C 0 :+: x) = simp x
simp (CI 0 :+: x) = simp x
simp (x :+: y) = 
  case (simp x,simp y) of
    (CI 0,CI 0) -> CI 0
    (CI 0,y') -> y'
    (x',CI 0) -> x'
    (x',y') -> x' :+: y'
    
simp (C a :*: C b) = C (a*b)
simp (CI a :*: CI b) = CI (a*b)
simp (_ :*: C 0) = CI 0
simp (x :*: C 1) = simp x
simp (_ :*: CI 0) = CI 0
simp (x :*: CI 1) = simp x
simp (C 0 :*: _) = CI 0
simp (C 1 :*: x) = simp x
simp (CI 0 :*: _) = CI 0
simp (CI 1 :*: x) = simp x
simp (x :*: y) =
  case (simp x,simp y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> CI 0
    (x',y') -> x' :*: y'


simp (C a :/: C b) = C (a/b)
--simp (CI a :/: CI b) = CI (a/b)
simp (_ :/: C 0) = error "divide by 0"
simp (x :/: C 1) = simp x
simp (_ :/: CI 0) = error "divide by 0"
simp (x :/: CI 1) = simp x
simp (C 0 :/: _) = CI 0
simp (C 1 :/: x) = CI 1 :/: simp x
simp (CI 0 :/: _) = CI 0
simp (CI 1 :/: x) = CI 1 :/: simp x
simp (x :/: y) =
  case (simp x,simp y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> error "divide by 0"
    (x',y') -> x' :/: y'

simp a = a

exp2val :: Exp -> Value

exp2val (InfixE (Just a) (VarE op) (Just b))
  | op == '(+) = exp2val a + exp2val b
  | op == '(-) = exp2val a - exp2val b
  | op == '(*) = exp2val a * exp2val b
  | op == '(/) = exp2val a / exp2val b
  | op == '(**) = exp2val a ** exp2val b
  | otherwise = error "can not parse"

exp2val (AppE (VarE fun) a)
  | fun ==  'log = Log $ exp2val a
  | fun ==  'sqrt = Sqrt $ exp2val a
  | fun ==  'exp = Exp $ exp2val a
  | fun ==  'sin = Sin $ exp2val a
  | fun ==  'cos = Cos $ exp2val a
  | fun ==  'tan = Tan $ exp2val a
  | fun ==  'asin = Asin $ exp2val a
  | fun ==  'acos = Acos $ exp2val a
  | fun ==  'atan = Atan $ exp2val a
  | fun ==  'sinh = Sinh $ exp2val a
  | fun ==  'cosh = Cosh $ exp2val a
  | fun ==  'tanh = Tanh $ exp2val a
  | fun ==  'asinh = Asinh $ exp2val a
  | fun ==  'acosh = Acosh $ exp2val a
  | fun ==  'atanh = Atanh $ exp2val a
  | fun ==  'negate = Neg $ exp2val a
  | otherwise = error "can not parse"
exp2val (LitE (IntegerL a)) = CI a
exp2val (LitE (RationalL a)) = C a
exp2val (VarE a) | a == 'pi = Pi
                 | otherwise = V a

val2exp :: Value -> Exp
val2exp (a :+: b) = (InfixE (Just (val2exp a)) (VarE '(+)) (Just (val2exp b)))
val2exp (a :-: b) = (InfixE (Just (val2exp a)) (VarE '(-)) (Just (val2exp b)))
val2exp (a :*: b) = (InfixE (Just (val2exp a)) (VarE '(*)) (Just (val2exp b)))
val2exp (a :/: b) = (InfixE (Just (val2exp a)) (VarE '(/)) (Just (val2exp b)))
val2exp (a :^: b) = (InfixE (Just (val2exp a)) (VarE '(**)) (Just (val2exp b)))

val2exp (Log a) = (AppE (VarE 'log) (val2exp a))
val2exp (Sqrt a) = (AppE (VarE 'sqrt) (val2exp a))
val2exp (Exp a) = (AppE (VarE 'exp) (val2exp a))
val2exp (Cos a) = (AppE (VarE 'cos) (val2exp a)) 
val2exp (Tan a) = (AppE (VarE 'tan) (val2exp a))
val2exp (Asin a) = (AppE (VarE 'asin) (val2exp a))
val2exp (Acos a) = (AppE (VarE 'acos) (val2exp a))
val2exp (Atan a) = (AppE (VarE 'atan) (val2exp a))
val2exp (Sinh a) = (AppE (VarE 'sinh) (val2exp a))
val2exp (Cosh a) = (AppE (VarE 'cosh) (val2exp a))
val2exp (Tanh a) = (AppE (VarE 'tanh) (val2exp a))
val2exp (Asinh a) = (AppE (VarE 'asinh) (val2exp a))
val2exp (Acosh a) = (AppE (VarE 'acosh) (val2exp a))
val2exp (Atanh a) = (AppE (VarE 'atanh) (val2exp a))
val2exp (Neg a) = (AppE (VarE 'negate) (val2exp a))


val2exp (CI a) = LitE (IntegerL a)
val2exp (C a) = LitE (RationalL a)
val2exp Pi = VarE 'pi
val2exp (V a) = VarE $ a

diffe :: Q Exp -> Q Exp -> Q Exp
diffe expr val = do
  e <- expr
  runIO $ writeFile "hoge1.txt" $ show e
  v <- val
  runIO $ writeFile "hoge2.txt" $ show v
  let b = diff' (exp2val e) (exp2val v)
  runIO $ writeFile "hoge4.txt" $ show b
  let a =  val2exp b
  runIO $ writeFile "hoge3.txt" $ show a
  return a

stre :: Q Exp -> Q Exp
stre expr = do
  v <- expr
  stringE $ show v

strv :: Q Exp -> Q Exp
strv expr = do
  v <- expr
  stringE $ show $ exp2val v

