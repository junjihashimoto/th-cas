
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import CAS

orgf :: Value
orgf =
  let x = V "x"
      y = V "y"
  in
   x*x + x*y+ y 

orgf' :: Value
orgf' =
  let x = V "x"
      y = V "y"
  in
   diff orgf x

hogedec :: String -> Value -> Q [Dec]
hogedec x y = hogeQ

--func :: Double -> Double -> Double
--func x y = [dif|(x ** 3) + x * y + 3|]

--hogeQ' :: String -> Value -> Q [Dec]
--hogeQ' func val = runQ [hs| hoge x y = x + y |]

main :: IO ()
main = do
  let x :: Value
      x = V "x"
      y :: Value
      y = V "y"
      -- z :: Value
      -- z = V "y"
      v = (x ** 3) + x * y + 3 :: Value
      b = 3
  print [heredoc|1+2|]
  print $(primeQ 0 67)
  print v
  print $ diff' v x
