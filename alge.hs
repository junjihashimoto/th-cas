
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import CAS
import Control.Applicative

--hog  x y = $(diffe [| if x < 3 then x*x else  x*y|] [|x|])
hoge x y = $(stre (diffe [|x*x + x*y|] [|x|]))
hoge2 x y = $(strv (diffe [|x*x + x*y|] [|x|]))

-- aaa = do
--   let x = 0
--       y = 0
--   runQ $ [|x*x + x*y|]

main = do
  -- v <- aaa
  -- print $ v
--  print $ hog 3 2
  print $ hoge 3 2
  print $ hoge2 3 2
