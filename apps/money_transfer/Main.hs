module Main where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text.IO                  as T
import qualified Data.Trie.Text                as Trie


import           CERES.Operate
import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Environment
import           CERES.BI.Interpret
import           CERES.BI.Util.Random


main :: IO ()
main = do
  T.putStrLn "CEREScript-Core Basic Interpreter"
  T.putStrLn "Test by Money Transfer scenario"
  T.putStrLn " - Initializing World"
  iWorld <- initializer
  let newWorld = runSimulator 16 iWorld
  print newWorld
  T.putStrLn "End"

initializer = do
  T.putStrLn " - = Set Spools"
  let iSpools = IM.empty
  T.putStrLn " - = Set ValueList"
  let iValueList = IM.empty
  T.putStrLn " - = Set WorldState"
  T.putStrLn " - = + Set HistoricalTable"
  let iHistoricalTable = IM.empty
  T.putStrLn " - = + Set NHistoricalTable"
  let iNHistoricalTable = IM.empty
  let iValueMap      = blankVM
  let iTrie          = blankVNM
  let rGen           = mkGenFromInt 0
  let iWorldState =
        WorldState Nothing iHistoricalTable iNHistoricalTable iValueMap iTrie iValueMap iTrie rGen
  T.putStrLn " - = Set SpoolInstances"
  let iSITable = IM.empty
  let iWorld = World iSpools iValueList iWorldState iSITable 0 256
  return iWorld

