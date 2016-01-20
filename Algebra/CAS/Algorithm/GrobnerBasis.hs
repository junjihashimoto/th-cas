{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.GrobnerBasis where

import Algebra.CAS.Type

lcmGB :: Value -> Value -> Value
lcmGB a b = lcmV ca cb * lcmGB' va vb
  where
    (ca,va) = headV a
    (cb,vb) = headV b

lcmGB' :: Value -> Value -> Value
lcmGB' 1 1 = 1
lcmGB' a 1 = a
lcmGB' 1 a = a
lcmGB' a b = 
  if hva == hvb
  then lcmGB' ta tb * (hva ** max hpa hpb)
  else if hva < hvb
       then lcmGB' a tb * (hvb ** hpb)
       else lcmGB' ta b * (hva ** hpa)
  where
    (hva,hpa) = splitExp $ headMul a
    (hvb,hpb) = splitExp $ headMul b
    ta = tailMul a
    tb = tailMul b

divAllGB :: Value -> Value -> Value
divAllGB a b = expand $ t + (h/b)
  where
    h = headAdd a
    t = case (tailAdd a) of
      0 -> 0
      v -> divAllGB v b


sGB f g = expand $ divAllGB (expand $ expand $ (ca*cb)*(lcmGB va vb)*f) (headAdd f)
                 - divAllGB (expand $ expand $ (ca*cb)*(lcmGB va vb)*g) (headAdd g)
  where
    (ca,va) = headV f
    (cb,vb) = headV g

divs :: Value -> Value -> Bool
divs f g = va == lcm'
  where
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' = lcmGB va vb

reduction :: Value -> Value -> (Value,Value)
reduction f g =
  if va == lcm'
  then
    let (a,b) = reduction (expand (f - c*g)) g
    in (c+a,b)
  else
    case mt of
    0 -> (0,h)
    t -> let (a,b) = reduction t g
         in (a,b+h)
  where
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' = lcmGB va vb
    h = headAdd f
    mt = tailAdd f
    c = (lcm' / vb)*ca/cb

reductions :: Value -> [Value] -> Value
reductions f [] = f
reductions f (g:gs) =
  let (a,b) = reduction f g
  in case b of
     0 -> 0
     c -> expand $ reductions (expand c) gs

allPair [] = []
allPair (x:xs) = map (\x' -> (x,x')) xs ++ allPair xs
  
grobnerG :: [Value] -> [Value]
grobnerG formulas = filter ((/=) 0) $ map (uncurry sGB) $ allPair formulas


grobnerBasis :: [Value] -> [Value]
grobnerBasis formulas = map lc1 $ grobnerBasis' formulas $ allPair formulas
{-  where
    fs = 
    fs' = map headAdd fs
    fs'' =  map (\f -> lcmGB f ) fs'
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' f g = lcmGB va vb
-}

lc1 :: Value -> Value
lc1 formula = expand $ expand $ formula / ca
  where
    (ca,va) = headV formula

grobnerBasis' :: [Value] -> [(Value,Value)] -> [Value]
grobnerBasis' formulas [] = formulas
grobnerBasis' formulas aa@((a,b):other) =
  case reductions (sGB a b) formulas of
  0 -> grobnerBasis' formulas other
  c -> grobnerBasis (formulas++[c])

grobnerBasisIO :: [Value] -> IO [Value]
grobnerBasisIO formulas = grobnerBasisIO' formulas $ allPair formulas

grobnerBasisIO' :: [Value] -> [(Value,Value)] -> IO [Value]
grobnerBasisIO' formulas [] = return formulas
grobnerBasisIO' formulas aa@((a,b):other) = do
  print "formulas"
  print formulas
  print "div"
  print aa
  print "a"
  print a
  print "b"
  print b
  print "sGB"
  print (sGB a b)
  print "r"
  print (reductions (sGB a b) formulas)
  case reductions (sGB a b) formulas of
    0 -> grobnerBasisIO' formulas other
    c -> grobnerBasisIO (formulas++[c])

(x,y,z)=(val "x",val "y",val "z")

fs = [((1 + ((-3) * (x ** 2))) + (x ** 3)) + ((-1) * y),
      ((-1) + ((-1) * (x ** 2))) + (y ** 2)
     ]
