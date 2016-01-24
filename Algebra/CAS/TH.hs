{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

module Algebra.CAS.TH where

import Language.Haskell.TH.Syntax

import Algebra.CAS.Base
import qualified Algebra.CAS.Algorithm.Diff as A
import qualified Language.Haskell.TH.Ppr as P
import qualified Data.Text as T

exp2val :: Exp -> Formula

exp2val (InfixE (Just a) (VarE op) (Just b))
  | op == '(+) = exp2val a + exp2val b
  | op == '(-) = exp2val a - exp2val b
  | op == '(*) = exp2val a * exp2val b
  | op == '(/) = exp2val a / exp2val b
  | op == '(**) = exp2val a ** exp2val b
  | otherwise = error "exp2val // can not parse"

exp2val (AppE (VarE fun) a)
  | fun ==  'log = S $ Log $ exp2val a
  | fun ==  'sqrt = S $ Sqrt $ exp2val a
  | fun ==  'exp = S $ Exp $ exp2val a
  | fun ==  'sin = S $ Sin $ exp2val a
  | fun ==  'cos = S $ Cos $ exp2val a
  | fun ==  'tan = S $ Tan $ exp2val a
  | fun ==  'asin = S $ Asin $ exp2val a
  | fun ==  'acos = S $ Acos $ exp2val a
  | fun ==  'atan = S $ Atan $ exp2val a
  | fun ==  'sinh = S $ Sinh $ exp2val a
  | fun ==  'cosh = S $ Cosh $ exp2val a
  | fun ==  'tanh = S $ Tanh $ exp2val a
  | fun ==  'asinh = S $ Asinh $ exp2val a
  | fun ==  'acosh = S $ Acosh $ exp2val a
  | fun ==  'atanh = S $ Atanh $ exp2val a
  | fun ==  'negate = C (CI (-1)) * (exp2val a)
  | otherwise = error "can not parse"
exp2val (LitE (IntegerL a)) = C (CI a)
exp2val (LitE (RationalL a)) = C (CR (fromRational a))
exp2val (VarE a@(Name (OccName v) _)) | a == 'pi = Pi
                                      | otherwise = V v

exp2val a@_ = error $ "exp2val // can not parse:" ++ show a

val2exp :: Formula -> Exp
val2exp (a :+: b) = (InfixE (Just (val2exp a)) (VarE '(+)) (Just (val2exp b)))
val2exp (a :*: b) = (InfixE (Just (val2exp a)) (VarE '(*)) (Just (val2exp b)))
val2exp (a :/: b) = (InfixE (Just (val2exp a)) (VarE '(/)) (Just (val2exp b)))
val2exp (a :^: b) = (InfixE (Just (val2exp a)) (VarE '(**)) (Just (val2exp b)))

val2exp (S (Log a)) = (AppE (VarE 'log) (val2exp a))
val2exp (S (Sqrt a)) = (AppE (VarE 'sqrt) (val2exp a))
val2exp (S (Exp a)) = (AppE (VarE 'exp) (val2exp a))
val2exp (S (Sin a)) = (AppE (VarE 'sin) (val2exp a)) 
val2exp (S (Cos a)) = (AppE (VarE 'cos) (val2exp a)) 
val2exp (S (Tan a)) = (AppE (VarE 'tan) (val2exp a))
val2exp (S (Asin a)) = (AppE (VarE 'asin) (val2exp a))
val2exp (S (Acos a)) = (AppE (VarE 'acos) (val2exp a))
val2exp (S (Atan a)) = (AppE (VarE 'atan) (val2exp a))
val2exp (S (Sinh a)) = (AppE (VarE 'sinh) (val2exp a))
val2exp (S (Cosh a)) = (AppE (VarE 'cosh) (val2exp a))
val2exp (S (Tanh a)) = (AppE (VarE 'tanh) (val2exp a))
val2exp (S (Asinh a)) = (AppE (VarE 'asinh) (val2exp a))
val2exp (S (Acosh a)) = (AppE (VarE 'acosh) (val2exp a))
val2exp (S (Atanh a)) = (AppE (VarE 'atanh) (val2exp a))

val2exp (C (CI a)) = LitE (IntegerL a)
val2exp (C (CR a)) = LitE (RationalL (toRational a))
val2exp (C (CF a b)) = LitE (RationalL ((toRational a)/(toRational b)))
val2exp (C One) = LitE (IntegerL 1)
val2exp (C Zero) = LitE (IntegerL 0)
val2exp Pi = VarE 'pi
val2exp (V a) = VarE $ mkName a

val2exp a@_ = error $ "val2exp // can not parse:" ++ show a

lift  ::  Formula -> Exp
lift  = val2exp
lift1 ::  (Formula -> Formula) -> Exp -> Exp
lift1 a b = val2exp $ a (exp2val b)
lift2 ::  (Formula -> Formula -> Formula) -> Exp -> Exp -> Exp
lift2 a b c = val2exp $ a (exp2val b) (exp2val c)
lift3 ::  (Formula -> Formula -> Formula -> Formula) -> Exp -> Exp -> Exp -> Exp
lift3 a b c d = val2exp $ a (exp2val b) (exp2val c) (exp2val d)

prettyPrint ::  Formula ->  String
prettyPrint var = T.unpack $
                  T.replace "GHC.Num." "" $
                  T.replace "GHC.Float." "" $
                  T.replace "GHC.Real." "" $
                  T.pack $ show $ P.ppr $ val2exp var

--deriving Show Formula where
--  show a = prettyPrint a

diff :: Q Exp -> Q Exp -> Q Exp
diff a b = (lift2 A.diff) <$> a <*> b
