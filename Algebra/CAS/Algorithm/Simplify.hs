{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Simplify where

import Algebra.CAS.Type
import Data.List


-- | simplify operations of constant values
-- >>> import Algebra.CAS.Core(prettyPrint)
-- >>> let x = "x" :: Value
-- >>> prettyPrint $ simpConst $ x * 0
-- "0"
-- >>> prettyPrint $ simpConst $ x * 1
-- "x"
-- >>> prettyPrint $ simpConst $ 3 + 3
-- "6"
simpConst :: Value -> Value
simpConst (Neg v) = CI (-1) :*: simpConst v
simpConst (C a :+: C b) = C (a+b)
simpConst (CI a :+: CI b) = CI (a+b)
simpConst (x :+: C 0) = simpConst x
simpConst (x :+: CI 0) = simpConst x
simpConst (C 0 :+: x) = simpConst x
simpConst (CI 0 :+: x) = simpConst x
simpConst (x :+: y) = 
  case (simpConst x,simpConst y) of
    (CI 0,CI 0) -> CI 0
    (CI 0,y') -> y'
    (x',CI 0) -> x'
    (x',y') -> x' :+: y'

simpConst (C a :-: C b) = C (a-b)
simpConst (CI a :-: CI b) = CI (a-b)
simpConst (x :-: C 0) = simpConst x
simpConst (x :-: CI 0) = simpConst x
simpConst (C 0 :-: x) = CI (-1) * simpConst x
simpConst (CI 0 :-: x) = CI (-1) * simpConst x
simpConst (x :-: y) = 
  case (simpConst x,simpConst y) of
    (CI 0,CI 0) -> CI 0
    (CI 0,y') -> CI (-1) * y'
    (x',CI 0) -> x'
    (x',y') -> x' :+: (CI (-1)) * y'

simpConst (C a :*: C b) = C (a*b)
simpConst (CI a :*: CI b) = CI (a*b)
simpConst (_ :*: C 0) = CI 0
simpConst (x :*: C 1) = simpConst x
simpConst (_ :*: CI 0) = CI 0
simpConst (x :*: CI 1) = simpConst x
simpConst (C 0 :*: _) = CI 0
simpConst (C 1 :*: x) = simpConst x
simpConst (CI 0 :*: _) = CI 0
simpConst (CI 1 :*: x) = simpConst x
simpConst c@(V a :*: V b) | a == b = V a :^: CI 2
                          | otherwise = c

simpConst e@((V a :^: CI b) :*: V c) | a ==  c = V a :^: CI (b+1)
                                     | otherwise = e

simpConst e@(V a :*: (V b :^: CI c) ) | a ==  b = V a :^: CI (c+1)
                                      | otherwise = e

simpConst e@((V a :^: CI b) :*: (V c :^: CI d) ) | a ==  c = V a :^: CI (b+d)
                                                 | otherwise = e
simpConst (x :*: y) =
  case (simpConst x,simpConst y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> CI 0
    (x',y') -> x' :*: y'


simpConst (C a :/: C b) = C (a/b)
simpConst (CI a :/: CI b) = CI (a `div` b)
simpConst (_ :/: C 0) = error "divide by 0"
simpConst (x :/: C 1) = simpConst x
simpConst (_ :/: CI 0) = error "divide by 0"
simpConst (x :/: CI 1) = simpConst x
simpConst (C 0 :/: _) = CI 0
simpConst (C 1 :/: x) = CI 1 :/: simpConst x
simpConst (CI 0 :/: _) = CI 0
simpConst (CI 1 :/: x) = CI 1 :/: simpConst x
simpConst (x :/: y) =
  case (simpConst x,simpConst y) of
    (CI 0,_) -> CI 0
    (_ ,CI 0) -> error "divide by 0"
    (x',y') -> x' :/: y'

simpConst a = a


-- | polinomial adder
-- >>> import Algebra.CAS.Core(prettyPrint)
-- >>> let x = "x" :: Value
-- >>> prettyPrint $ addPoly $ 2*x+x
-- "3 * x"
addPoly ::  Value ->  Value
addPoly val =
  let vals :: [[Value]]
      vals = map destructMult $ destructAdd  val
      vals' ::  [ValueWithConst]
      vals' = groupByValue $ map splitConst vals
  in (converge simpConst) $ foldr1 (:+:) $ map toValue vals' 


-- | polinomial divider
divPoly ::  Value ->  Value
divPoly val =
  let vals :: [[Value]]
      vals = map destructMult $ destructAdd  val
      vals' ::  [ValueWithConst]
      vals' = groupByValue $ map splitConst vals
  in foldr1 (:+:) $ map toValue vals' 

converge ::  (Value ->  Value) -> Value -> Value
converge func val =
  let v' = func val
  in if v' ==  val
     then v'
     else converge func v'

simp ::  Value -> Value
simp = (converge simpConst).addPoly.(converge simpConst)

destructAdd ::  Value -> [Value]
destructAdd (x :+: y) = destructAdd x ++ destructAdd y
destructAdd x = [x]

destructMult ::  Value ->  [Value]
destructMult (x :*: y) = destructMult x ++ destructMult y
destructMult (x :/: y) = destructMult x ++ map (\v ->  CI 1 :/: v) (destructMult y)
destructMult x = [x]


-- | When formula does not include variable,
-- isConst returns True.
-- >>> import Algebra.CAS.Core(prettyPrint)
-- >>> let x = "x" :: Value
-- >>> isConst x
-- False
-- >>> isConst $ sin(x)*3
-- False
-- >>> isConst $ 3.0 * sin(3.0)
-- True
isConst :: Value ->  Bool
isConst (CI _) = True
isConst (C _) = True
isConst (V _) = False
isConst (Neg v) = isConst v
isConst (Sin v) = isConst v
isConst (Cos v) = isConst v
isConst (Tan v) = isConst v
isConst (Sinh v) = isConst v
isConst (Cosh v) = isConst v
isConst (Tanh v) = isConst v
isConst (Asin v) = isConst v
isConst (Acos v) = isConst v
isConst (Atan v) = isConst v
isConst (Asinh v) = isConst v
isConst (Acosh v) = isConst v
isConst (Atanh v) = isConst v
isConst (Exp v) = isConst v
isConst (Log v) = isConst v
isConst (Abs v) = isConst v
isConst (Sig v) = isConst v
isConst (LogBase v0 v1) = isConst v0 &&  isConst v1
isConst Pi = True
isConst (Sqrt v) = isConst v
isConst (Diff v0 v1) = isConst v0 &&  isConst v1
isConst e@(Other _) = error $ "can not check isConst of " ++ show e
isConst (v0 :^: v1) = isConst v0 &&  isConst v1
isConst (v0 :*: v1) = isConst v0 &&  isConst v1
isConst (v0 :+: v1) = isConst v0 &&  isConst v1
isConst (v0 :-: v1) = isConst v0 &&  isConst v1
isConst (v0 :/: v1) = isConst v0 &&  isConst v1

data ValueWithConst =
  ValueWithConst {
    v_const ::  [[Value]]
  , v_value ::  [Value]
  } deriving (Show,Eq,Ord)

toValue ::  ValueWithConst ->  Value
toValue val =
  let consts ::  [Value]
      consts = map (\v ->  foldr (:*:) (CI 1) v) (v_const val)
  in (foldr (:+:) (CI 0) consts) :*:
     (foldr (:*:) (CI 1) (v_value val))


splitConst ::  [Value] ->  ValueWithConst
splitConst vals =
  ValueWithConst {
    v_const = [ifNull (filter isConst vals)]
  , v_value = sort (filter (not.isConst) vals)
  }
  where
    ifNull val = if val ==  [] then [CI 1] else val

groupByValue ::  [ValueWithConst] ->  [ValueWithConst]
groupByValue vals =  map merge $ groupBy (\a b ->  v_value a ==  v_value b) $ sortBy (\a b -> compare a b) vals
  where
    merge :: [ValueWithConst] ->  ValueWithConst
    merge vals@(x:xs) = 
      ValueWithConst {
        v_const = foldr (++) [] $ map v_const vals
      , v_value = v_value x
      }
