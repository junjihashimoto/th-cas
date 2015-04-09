{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module CAS where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.Meta (parseExp, parseDecs, parsePat)
import Language.Haskell.Meta.Utils (pretty)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data Value =
   V String
 | C Double
 | CI Integer
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
 deriving (Show,Read,Eq)

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
diff (x :^: CI 2) z = x * (diff x z)
diff (x :^: CI n) z = (x ** (fromIntegral (n-1))) * (diff x z)
diff (Sin x') y' = (Cos x') * (diff x' y')
diff (Cos x') y' = -1 * (Sin x') * (diff x' y')
diff (Exp x') y' = (Exp x') * (diff x' y')

diff (CI _) _ = CI 0
diff (C _) _ = CI 0
diff Pi _ = CI 0
diff (Log x') y' = recip x' * diff x' y'

diff a b = error $ "can not parse : " ++ show a ++ " ##  " ++ show b

integrate :: Value -> Value -> Value
-- integrate (V x') (V y') | x' == y' = 
--                         | otherwise = CI 0
integrate (x :+: y) z = (integrate x z) + (integrate y z)
integrate (a@(CI _) :*: y) z = a * integrate y z
integrate (a@(C _) :*: y) z = a * integrate y z
integrate (CI a) z = (CI a) * z
integrate (C a) z = (C a) * z
--integrate (z :^: CI n) z = (x ** (fromIntegral (n-1))) * (integrate x z)
integrate (Sin x') y' = (Cos x') * (integrate x' y')
integrate (Cos x') y' = (Sin x') * (integrate x' y')

integrate (CI _) _ = CI 0
integrate (C _) _ = CI 0
integrate (Log x') y' = recip x' * integrate x' y'

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



--expr =  [|1+1|]
-- multi :: Name -> ExpQ
-- multi n = [| ($(varE n) *) |]

-- main :: IO ()
-- main = do
--   let x :: Value
--       x = V "x"
--       y :: Value
--       y = V "y"
--       -- z :: Value
--       -- z = V "y"
--       v = (x ** 3) + x * y + 3 :: Value
--   print v
--   -- let a = 3
--   --     b =  $(multi 'a) 3
--   -- print $ b
-- dif :: QuasiQuoter
-- dif = QuasiQuoter {
--   quoteExp = 
--   }

isPrime :: (Integral a) => a -> Bool
isPrime k | k <=1 = False | otherwise = not $ elem 0 (map (mod k)[2..k-1])

nextPrime :: (Integral a) => a -> a
nextPrime n | isPrime n = n | otherwise = nextPrime (n+1)

doPrime :: (Integral a) => a -> a -> [a]
doPrime n m
     | curr > m = []
     | otherwise = curr:doPrime (curr+1) m
     where curr = nextPrime n

primeQ :: Int -> Int -> Q Exp
primeQ n m = [| doPrime n m |]


hs :: QuasiQuoter
hs =  QuasiQuoter {
 quoteExp = (either fail return . parseExp)
-- quoteDec = (either fail return . parseDecs)
 -- , quoteExp = either fail transformE . parseExp
  }

hogeQ :: Q [Dec]
hogeQ = runQ [d| hoge x y = x + y |]

-- hogeQ' :: String -> Value -> Q [Dec]
-- hogeQ' func val = runQ [hs| hoge x y = x + y |]

-- hogeQ'' :: String -> Q [Dec]
-- hogeQ'' val =
--   let h = hs val
--   in runQ [h| dummy |]


-- hogeQ :: Q [Dec]
-- hogeQ = runQ [d| hoge x y = x + y |]

--share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
--share fs x = fmap mconcat $ mapM ($ x) fs
