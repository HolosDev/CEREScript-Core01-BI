module CERES.BI.Interpret where


import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Spool
import           CERES.BI.Interpret.Cache

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe

import           Debug


runSimulator :: Time -> World -> World
runSimulator endTime = runSimulatorSub
 where
  runSimulatorSub aWorld = if currentTime > endTime
    then aWorld
    else runTimeSlot newWorld
   where
    currentTime = worldTime aWorld
    newWorld    = runTimeSlot aWorld


-- NOTE: Assume that the current EpochRow exists
runTimeSlot :: World -> World
runTimeSlot aWorld@World {..} = newWorld
 where
  aSpoolForest    = siAggregator aWorld
  resultList      = map (runSpoolTree aWorld) aSpoolForest
  siisList        = concatMap fst resultList
  wcList          = map snd resultList
  -- TODO: Change cacheCommitter style or union WorldCache in wcList
  committed       = foldr cacheCommitter worldState wcList
  newWorldHistory = IM.insert (worldTime + 1) newNextEpochRow targetWorldHistory
   where
    targetWorldHistory = worldHistory committed
    deltaValues = maybe IM.empty values mNextEpochRow
      where mNextEpochRow = IM.lookup (worldTime + 1) targetWorldHistory
    theNextValues = IM.union deltaValues (values currentEpochRow)
      where currentEpochRow = (worldHistory committed) IM.! worldTime
    newNextEpochRow = EpochRow (worldTime + 1) theNextValues
  newWorldState = committed { worldHistory = newWorldHistory }
  newSITable    = undefined worldSITable siisList
  newWorld      = aWorld { worldState   = newWorldState
                         , worldSITable = newSITable
                         , worldTime    = worldTime + 1
                         }


runSpoolTree :: World -> SpoolTree -> ([(SIIS, SpoolInstance)], WorldCache)
runSpoolTree aWorld@World {..} aSpoolTree@SpoolTree {..} =
  (siisList, newWorldCache)
 where
  worldCache                = cacheMaker aSpoolTree aWorld
  (siisList, newWorldCache) = foldr
    (\s (l, c) -> joint l (runSpoolInstance aWorld s c))
    ([], worldCache)
    siList
  joint aList (s, aWorldCache) = (s : aList, aWorldCache)


runSpoolInstance
  :: World -> SpoolInstance -> WorldCache -> ((SIIS, SpoolInstance), WorldCache)
runSpoolInstance = notYetImpl "runSpoolInstance"