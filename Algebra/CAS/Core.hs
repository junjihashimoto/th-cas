{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

module Algebra.CAS.Core where

import Language.Haskell.TH
import qualified Language.Haskell.TH.Ppr as P
import qualified Data.Text as T
import Algebra.CAS.Type

exp2val :: Exp -> Value

exp2val (InfixE (Just a) (VarE op) (Just b))
  | op == '(+) = exp2val a + exp2val b
  | op == '(-) = exp2val a - exp2val b
  | op == '(*) = exp2val a * exp2val b
  | op == '(/) = exp2val a / exp2val b
  | op == '(**) = exp2val a ** exp2val b
  | otherwise = error "exp2val // can not parse"

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
  | fun ==  'negate = neg :*: (exp2val a)
  | otherwise = error "can not parse"
exp2val (LitE (IntegerL a)) = CI a
exp2val (LitE (RationalL a)) = C a
exp2val (VarE a) | a == 'pi = Pi
                 | otherwise = V a

exp2val a@_ = error $ "exp2val // can not parse:" ++ show a

val2exp :: Value -> Exp
val2exp (a :+: b) = (InfixE (Just (val2exp a)) (VarE '(+)) (Just (val2exp b)))
val2exp (a :-: b) = (InfixE (Just (val2exp a)) (VarE '(-)) (Just (val2exp b)))
val2exp (a :*: b) = (InfixE (Just (val2exp a)) (VarE '(*)) (Just (val2exp b)))
val2exp (a :/: b) = (InfixE (Just (val2exp a)) (VarE '(/)) (Just (val2exp b)))
val2exp (a :^: b) = (InfixE (Just (val2exp a)) (VarE '(**)) (Just (val2exp b)))

val2exp (Log a) = (AppE (VarE 'log) (val2exp a))
val2exp (Sqrt a) = (AppE (VarE 'sqrt) (val2exp a))
val2exp (Exp a) = (AppE (VarE 'exp) (val2exp a))
val2exp (Sin a) = (AppE (VarE 'sin) (val2exp a)) 
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

val2exp (CI a) = LitE (IntegerL a)
val2exp (C a) = LitE (RationalL a)
val2exp One = LitE (IntegerL 1)
val2exp Zero = LitE (IntegerL 0)
val2exp Pi = VarE 'pi
val2exp (V a) = VarE $ a

val2exp a@_ = error $ "val2exp // can not parse:" ++ show a

lift  ::  Value -> Exp
lift  = val2exp
lift1 ::  (Value -> Value) -> Exp -> Exp
lift1 a b = val2exp $ a (exp2val b)
lift2 ::  (Value -> Value -> Value) -> Exp -> Exp -> Exp
lift2 a b c = val2exp $ a (exp2val b) (exp2val c)
lift3 ::  (Value -> Value -> Value -> Value) -> Exp -> Exp -> Exp -> Exp
lift3 a b c d = val2exp $ a (exp2val b) (exp2val c) (exp2val d)

prettyPrint ::  Value ->  String
prettyPrint var = T.unpack $
                  T.replace "GHC.Num." "" $
                  T.replace "GHC.Float." "" $
                  T.replace "GHC.Real." "" $
                  T.pack $ show $ P.ppr $ val2exp var
