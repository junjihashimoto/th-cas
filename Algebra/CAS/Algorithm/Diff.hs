{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Diff where

import Algebra.CAS.Type
import Algebra.CAS.Core
import Algebra.CAS.Algorithm.Simplify


diff :: Value -> Value -> Value
diff (V x') (V y') | x' == y' = One
                   | otherwise = Zero
diff (x :+: y) z = (diff x z) + (diff y z)
diff (x :-: y) z = (diff x z) - (diff y z)
diff (x :*: y) z = (diff x z) * y + x * (diff y z)
diff (x :/: y) z = ((diff x z) * y - x * (diff y z)) / (y * y)
diff (x :^: One) z = diff x z
diff (x :^: CI 1) z = diff x z
diff (x :^: CI 2) z = 2 * x * (diff x z)
diff (x :^: CI n) z = (fromIntegral n) * (x ** (fromIntegral (n-1))) * (diff x z)
diff (Sin x') y' = (Cos x') * (diff x' y')
diff (Cos x') y' = neg :*: ((Sin x') * (diff x' y'))
diff (Exp x') y' = (Exp x') * (diff x' y')

diff (CI _) _ = Zero
diff (C _) _ = Zero
diff Pi _ = Zero
diff (Log x') y' = recip x' * diff x' y'

diff a b = error $ "diff //  can not parse : " ++ show a ++ " ##  " ++ show b

