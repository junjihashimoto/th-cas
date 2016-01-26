module Main where

import Test.DocTest

main :: IO ()
main = do
  doctest $
    [
    "-XOverloadedStrings",
    "Algebra/CAS.hs",
    "Algebra/CAS/Diff.hs",
    "Algebra/CAS/TH.hs",
    "Algebra/CAS/Base.hs"
    ]
