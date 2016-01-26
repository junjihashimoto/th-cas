module Algebra.CAS.Solve where

import Algebra.CAS.Base
import qualified Data.Map as M
import Control.Applicative

match :: Formula -- ^ pattern
       -> Formula -- ^ matched formula
       -> Maybe [(Formula,Formula)] -- ^ matched variable pairs
match a b = match' a b []
  where
    match' :: Formula
           -> Formula
           -> [(Formula,Formula)]
           -> Maybe [(Formula,Formula)]
    match' (a0:^:b0) (a1:^:b1) lst = do
      v1 <- match' a0 a1 lst
      case (b0,b1) of
        (c0@(C _),c1@(C _)) ->
          if c0 == c1
          then return lst
          else Nothing
        _ -> match' b0 b1 v1
    match' (a0:*:b0) (a1:*:b1) lst = do
      v <- match' b0 b1 lst
      match' a0 a1 v
    match' (a0:+:b0) (a1:+:b1) lst = do
      v <- match' b0 b1 lst
      match' a0 a1 v
    match' (a0:/:b0) (a1:/:b1) lst = do
      v <- match' b0 b1 lst
      match' a0 a1 v
    match' (_:^:_) _ _ = Nothing
    match' (a0:+:b0) a1 lst = do
      v <- match' b0 a1 lst
      match' a0 0 v
    match' (a0:*:b0) a1 lst = do
      v <- match' b0 a1 lst
      match' a0 1 v
    match' (_:/:_) _ _ = Nothing
    match' (V x) (V y) lst | x == y = Just $ lst
                           | otherwise = Nothing
    match' a0@(CV _) a1 lst = Just $ (a0,a1):lst
    match' _ _ _ = Nothing
  
solve1 :: Formula -> Formula -> Maybe [Formula]
solve1 f v = do
  let [a,b] = map CV ["a","b"]
  list <- match (a*v+b) f
  let m = M.fromList list
  return [- (m M.! b /  m M.! a)]

solve2 :: Formula -> Formula -> Maybe [Formula]
solve2 f v = abc <|> ac
  where
    ac = do 
      let [a,b,c] = map CV ["a","b","c"]
      list <- (match (a*v**2+c) f)
      let m = M.fromList ((b,0):list)
      let [a',b',c'] = map (m M.!) [a,b,c]
      anser a' b' c'
    abc = do 
      let [a,b,c] = map CV ["a","b","c"]
      list <- (match (a*v**2+b*v+c) f)
      let m = M.fromList list
      let [a',b',c'] = map (m M.!) [a,b,c]
      anser a' b' c'
    anser a' b' c' = do
      let a2= 2*a'
      let sq=sqrt $ expand (b'**2-4*a'*c')
      return $ [(sq-b')/a2,(-sq-b')/a2]

solve :: Formula -- ^ formula
      -> Formula -- ^ variable
      -> Maybe [Formula] -- ^ answer
solve f v = solve2 f v <|> solve1 f v


linsolve :: [Formula] -- ^ formulas
         -> Maybe [(Formula,Formula)] -- ^ answer
linsolve fs = Just $ rSolve r
  where
    r = reverse $ lReductions fs

lReduction :: Formula
           -> Formula
           -> Formula
lReduction f0 f1 =
  if t0 == t1
  then expand $ f1 - (c1/c0)*f0
  else f1
  where
    (c0,t0) = headV f0
    (c1,t1) = headV f1


lReductions :: [Formula]
            -> [Formula]
lReductions [] = []
lReductions (f:fs) = f:lReductions (flist f fs)
  where
    flist :: Formula -> [Formula] -> [Formula]
    flist f' fs' =  map (lReduction f') fs'

rSolve :: [Formula] -> [(Formula,Formula)]
rSolve [] = []
rSolve (f:fs) =
  case a of
  Just [a'] -> (v,a'): rSolve (map (subst [(v,a')]) fs)
  Just _ -> error "error"
  Nothing -> []
  where
    a = solve1 f v
    v = head $ variables f
