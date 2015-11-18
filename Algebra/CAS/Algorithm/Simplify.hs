{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Simplify where

import Algebra.CAS.Type
import Data.List

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

simpConst c@(V a :*: V b) | a == b = V a :^: CI 2
                          | otherwise = c

simpConst e@((V a :^: CI b) :*: V c) | a ==  c = V a :^: CI (b+1)
                                     | otherwise = e

simpConst e@(V a :*: (V b :^: CI c) ) | a ==  b = V a :^: CI (c+1)
                                      | otherwise = e

simpConst e@((V a :^: CI b) :*: (V c :^: CI d) ) | a ==  c = V a :^: CI (b+d)
                                                 | otherwise = e

simpConst a = a


simpAddPoly ::  Value ->  Value
simpAddPoly val =
  let vals :: [[Value]]
      vals = map destructMult $ destructAdd  val
      vals' ::  [ValueWithConst]
      vals' = groupByValue $ map splitConst vals
  in foldr1 (:+:) $ map toValue vals' 


simpLoop ::  (Value ->  Value) -> Value -> Value
simpLoop func val =
  let v' = func val
  in if v' ==  val
     then v'
     else simpLoop func v'

simp ::  Value -> Value
simp = (simpLoop simpConst).simpAddPoly.(simpLoop simpConst)

destructAdd ::  Value -> [Value]
destructAdd (x :+: y) = destructAdd x ++ destructAdd y
destructAdd x = [x]

destructMult ::  Value ->  [Value]
destructMult (x :*: y) = destructMult x ++ destructMult y
destructMult (x :/: y) = destructMult x ++ map (\v ->  CI 1 :/: v) (destructMult y)
destructMult x = [x]


isConst :: Value ->  Bool
isConst (C _) = True
isConst (CI _) = True
isConst Pi = True
isConst _ = False

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
