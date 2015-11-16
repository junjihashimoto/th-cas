{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

module Algebra.CAS.TH where

import Control.Applicative
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Algebra.CAS.Core
import qualified Algebra.CAS.Algorithm.Diff as A

diff :: Q Exp -> Q Exp -> Q Exp
diff a b = (lift2 A.diff) <$> a <*> b
