{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Diff where

import Algebra.CAS.Type
--import Algebra.CAS.Core
--import Algebra.CAS.Algorithm.Simplify


diff :: Value -> Value -> Value
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
diff (S (Log x')) y' = recip x' * diff x' y'

diff a b = error $ "diff //  can not parse : " ++ show a ++ " ##  " ++ show b

