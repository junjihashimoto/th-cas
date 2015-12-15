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
simpConst (CI 0) = Zero
simpConst (CI 1) = One
simpConst (C 0) = Zero
simpConst (C 1) = One
simpConst (a :+: b) = simpConst a + simpConst b
simpConst (a :-: b) = simpConst a - simpConst b
simpConst (a :*: b) = simpConst a * simpConst b
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
