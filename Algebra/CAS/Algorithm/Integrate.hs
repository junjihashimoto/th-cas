{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.Algorithm.Integrate where

import Algebra.CAS.Type
import Algebra.CAS.Core
import Algebra.CAS.Algorithm.Simplify


integrate :: Value -> Value -> Value
integrate (x :+: y) z = (integrate x z) + (integrate y z)
integrate (a@(CI _) :*: y) z = a * integrate y z
integrate (a@(C _) :*: y) z = a * integrate y z
integrate (CI a) z = (CI a) * z
integrate (C a) z = (C a) * z
integrate (Sin x') y' | x' == y' = Cos x'
                      | otherwise = error "can not parse"
integrate (Cos x') y' | x' == y' = -1 * Sin x'
                      | otherwise = error "can not parse"
integrate (x :^: CI 2) y | x == y    = x ** 3 / 3
                         | otherwise = error "can not parse"
integrate (x :^: CI n) y | x == y    = (x :^: (CI (n+1))) / (fromIntegral (n+1))
                         | otherwise = error "can not parse"

integrate a b = error $ "can not parse : " ++ show a ++ " ##  " ++ show b
