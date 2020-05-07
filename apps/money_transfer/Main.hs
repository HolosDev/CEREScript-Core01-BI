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
import           CERES.BI.Type


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
  let bWorld = blankWorld
  let theWorld = bWorld
  return theWorld
