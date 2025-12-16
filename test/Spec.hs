module Main (main) where

import Test.Hspec

import qualified Algebra.CAS.THSpec
import qualified Algebra.CAS.BasicSpec
import qualified Algebra.CAS.DiffSpec
import qualified Algebra.CAS.SolveSpec
import qualified Algebra.CAS.GrobnerBasisSpec
import qualified Algebra.CAS.IntegrateSpec

main :: IO ()
main = hspec $ do
  Algebra.CAS.THSpec.spec
  Algebra.CAS.BasicSpec.spec
  Algebra.CAS.DiffSpec.spec
  Algebra.CAS.SolveSpec.spec
  Algebra.CAS.GrobnerBasisSpec.spec
  Algebra.CAS.IntegrateSpec.spec
