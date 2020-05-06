import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.HUnit
import Test.QuickCheck

import Test.CERES.BI.Interpret.Instruction

main :: IO ()
main = do
  Test.defaultMain
    [ Test.CERES.BI.Interpret.Instruction.tests
    ]
