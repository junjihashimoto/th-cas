module Main where

import Test.DocTest

main :: IO ()
main = do
  -- p <-  runResourceT $ find "." (glob "*autogen/cabal_macros.h" <> regular) $$ do
  --   v <-  await
  --   case v of
  --     Just path ->  return ["-optP-include -optP" ++ path]
  --     Nothing ->  return []
  doctest $ --  p ++ 
    [
    "-XOverloadedStrings",
    "Algebra/CAS.hs",
    "Algebra/CAS/Algorithm/Diff.hs",
    "Algebra/CAS/TH.hs",
    "Algebra/CAS/Core.hs",
    "Algebra/CAS/Type.hs"
    ]
