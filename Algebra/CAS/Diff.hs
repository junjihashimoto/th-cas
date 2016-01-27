{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Diff where

import Algebra.CAS.Base

-- | Partial derivative
-- >>> let [x,y] = map V ["x","y"]
-- >>> diff (x*y) x
-- y
-- >>> diff (sin(x)*y) x
-- y*(cos(x))
-- >>> diff (x^3) x
-- 3*(x^2)
diff :: Formula -> Formula -> Formula
diff (V x') (V y') | x' == y' = C One
                   | otherwise = C Zero
diff (x :+: y) z = (diff x z) + (diff y z)
diff (x :*: y) z = (diff x z) * y + x * (diff y z)
diff (x :/: y) z = ((diff x z) * y - x * (diff y z)) / (y * y)
diff (x :^: C One) z = diff x z
diff (x :^: C (CI 1)) z = diff x z
diff (x :^: C (CI 2)) z = 2 * x * (diff x z)
diff (x :^: C (CI n)) z = (fromIntegral n) * (x ** (fromIntegral (n-1))) * (diff x z)
diff (S (Sin x')) y' = (S (Cos x')) * (diff x' y')
diff (S (Cos x')) y' = - ((S (Sin x')) * (diff x' y'))
diff (S (Exp x')) y' = (S (Exp x')) * (diff x' y')

diff (C _) _ = C Zero
diff Pi _ = C Zero
diff (CV _) _ = C Zero
diff (S (Log x')) y' = recip x' * diff x' y'

diff a b = error $ "diff //  can not parse : " ++ show a ++ " ##  " ++ show b

diffn :: Integer -> Formula -> Formula -> Formula
diffn 0 a _ = a
diffn 1 a b = diff a b
diffn n a b | n < 0 = error $ "diffn can not do negative diff. n:" ++ show n
            | otherwise =  diffn (n-1) (diff a b) b
