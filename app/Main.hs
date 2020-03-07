module Main where


import CERES.Operate
import Data.CERES.Script
import Data.CERES.Operator
import Data.CERES.Type
import Data.CERES.Value

import CERES.BI.Data
import CERES.BI.Data.Environment
import CERES.BI.Interpret

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import qualified Data.Text.IO as T


main :: IO ()
main = do
  T.putStrLn "CEREScript-Core Basic Interpreter"
  T.putStrLn " - Initializing World"
  iWorld <- initializer
  let result = runWorld iWorld
  T.putStrLn "End"

initializer = do
  T.putStrLn " - = Set Spools"
  let iSpools = IM.empty
  T.putStrLn " - = Set ValueList"
  let iValueList = IM.empty
  T.putStrLn " - = Set WorldState"
  T.putStrLn " - = + Set HistoricTable"
  let iHistoricTable = IM.empty
  let iValueMap = IM.empty
  let iWorldState = WorldState Nothing iHistoricTable iValueMap iValueMap
  T.putStrLn " - = Set SpoolInstances"
  let iSITable = IM.empty
  let iWorld = World iSpools iValueList iWorldState iSITable 0
  let newWorld = runSimulator 16 iWorld
  print newWorld

runWorld = error "[TODO]<runWorld> Not yet implemented"
