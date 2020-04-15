module CERES.BI.Interpret where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe


import           Data.CERES.Data
import           Data.CERES.Data.Method
import           Data.CERES.Operator
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Cache
import           CERES.BI.Interpret.Instruction
import           CERES.BI.Interpret.Spool

import           CERES.BI.Type

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
  aSpoolForest     = siAggregator aWorld
  resultList       = map (runSpoolTree aWorld) aSpoolForest
  siisList         = concatMap fst resultList
  wcList           = map snd resultList
  -- TODO: Change cacheCommitter style or union WorldCache in wcList
  committed        = foldr cacheCommitter worldState wcList
  nextWorldTime    = worldTime + 1
  nextWorldHistory = IM.insert nextWorldTime newNextEpochRow targetWorldHistory
   where
    targetWorldHistory = worldHistory committed
    deltaValues        = maybe IM.empty values mNextEpochRow
      where mNextEpochRow = IM.lookup nextWorldTime targetWorldHistory
    theNextValues = IM.union deltaValues (values currentEpochRow)
     where
      currentEpochRow =
        fromMaybe (EpochRow worldTime IM.empty)
          . IM.lookup worldTime
          . worldHistory
          $ committed
    newNextEpochRow = EpochRow nextWorldTime theNextValues
  newWorldState = committed { worldHistory = nextWorldHistory }
  newSITable    = siisExecutor worldTime worldSITable siisList
  newWorld      = updateWorld aWorld newWorldState newSITable nextWorldTime


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


-- TODO: How to avoid SpoolInstance ID collision
runSpoolInstance
  :: World -> SpoolInstance -> WorldCache -> ((SIIS, SpoolInstance), WorldCache)
runSpoolInstance world@World {..} si@SI {..} wCache =
  ((siis, newSI), newWorldCache)
 where
  -- TODO: Change `StrValue "Retain"` as a named constant
  iLTCache = csInitLocalTemp $ worldSpools IM.! siSpoolID
  isResume = maybe False getBool $ IM.lookup resumeCodeIdx siLocalVars
  iLVCache =
    -- TODO: Not sure to initialize ExecutingTime variable
    (if isResume then id else IM.insert executingTimeIdx (IntValue 0))
      . IM.insert resumeCodeIdx (BoolValue False)
      $ siLocalVars
  -- FIXME
  iLNVCache   = undefined
  -- FIXME
  iLNTCache   = undefined
  iLocalCache = (iLVCache, iLNVCache, iLTCache, iLNTCache)
  (newCache@(newWorldCache, (newLVCache, newLNVCache, newLTCache, newLNTCache), newRG), restCEREScript)
    = runCEREScript world si (wCache, iLocalCache, siRG) siRestScript
  siisCode = maybe "Retain" getStr $ IM.lookup retainCodeIdx newLTCache
  (doAbolish, doInit, nextLocalVars, nextLocalNVars) = case siisCode of
    "Retain"  -> (False, False, newLVCache, newLNVCache)
    "Forget"  -> (False, False, blankVM, blankVNM)
    -- FIXME: Add SI initiation logic
    "Init"    -> (False, True, blankVM, blankVNM)
    "Abolish" -> (True, False, blankVM, blankVNM)
    _ -> error "[ERROR]<runSpoolInstance :=: _> Undefined Retention Code"
  -- NOTE: SIJump takes relative time-slot
  jumpTarget = maybe 1 getInt $ IM.lookup jumpOffsetIdx newLTCache
  siis       = if doAbolish || null (restCEREScript :: CEREScript)
    then SIEnd
    else SIJump jumpTarget
  nextCEREScript = if doInit then newCEREScript else restCEREScript
   where
    newCEREScript =
      maybe
          (  error
          $  "[ERROR]<runSpoolInstance :=: newCEREScript> No such SpoolID("
          ++ show siSpoolID
          ++ ") in worldSpools"
          )
          (\s -> runMaker (csScript s) (world,newCache))
        $ IM.lookup siSpoolID worldSpools
  newSI = si { siLocalVars  = nextLocalVars
             , siRestScript = nextCEREScript
             , siRG         = newRG
             }


runCEREScript
  :: World -> SpoolInstance -> Env -> CEREScript -> (Env, CEREScript)
runCEREScript aWorld@World {..} aSI@SI {..} = runCEREScriptSub
 where
  runCEREScriptSub cState [] = (cState, [])
  runCEREScriptSub cState@(wc@(hCache, nHCache, dCache, nDCache, vCache, nVCache), lc@(lVCache, lNVCache, lTCache, lNTCache), rg) (ceres : cScript)
    = if sp
      then ((nextWC, nextLC, nextRG), nextCEREScript)
      else runCEREScriptSub (nextWC, nextLC, nextRG) nextCEREScript
    -- NOTE: si == True, then end runCEREScript


   where
    (newWC, newLC, newRG) = runInstruction aWorld aSI cState ceres
    -- TODO: Check Stop or Pause
    spCode                = maybe "" getStr $ IM.lookup spCodeIdx newLTCache
    sp                    = spCode == "Stop" || spCode == "Pause"
    retentionCode         = case spCode of
      "Stop"  -> "Abolish"
      -- TODO: Not sure do I need to identify whether this is "Pause"
      "Pause" -> "Retain"
      _       -> maybe "Retain" getStr $ IM.lookup retainCodeIdx newLTCache
    -- TODO: Check the instruction is executed or not
    resumeFlag     = maybe False getBool $ IM.lookup resumeCodeIdx newLVCache
    nextCEREScript = if resumeFlag then (ceres : cScript) else cScript
    (newLVCache, newLNVCache, newLTCache, newLNTCache) = newLC
    nextWC         = newWC
    nextLVCache    = IM.insert retainCodeIdx (StrValue retentionCode) newLVCache
    nextLNVCache   = newLNVCache
    nextLTCache    = newLTCache
    nextLNTCache   = newLNTCache
    nextLC         = (nextLVCache, nextLNVCache, nextLTCache, nextLNTCache)
    nextRG         = newRG


runInstruction :: World -> SpoolInstance -> Env -> CERES -> Env
runInstruction aWorld aSI cState aCERES
  = case aCERES of
    (CRSInitVariable      vpA vpB)             -> crsInitVariable     aWorld aSI cState vpA vpB
    (CRSInitVariableAt    vpA vpB)             -> crsInitVariableAt   aWorld aSI cState vpA vpB
    (CRSSetValue          vpA vpB)             -> crsSetValue         aWorld aSI cState vpA vpB
    (CRSDeleteVariable    vp)                  -> crsDeleteVariable   aWorld aSI cState vp
    (CRSModifyValue       vpA vpB cOp)         -> crsModifyValue      aWorld aSI cState vpA vpB cOp
    (CRSCopyValue         vpA vpB)             -> crsCopyValue        aWorld aSI cState vpA vpB
    (CRSConvertValue      vp vt)               -> crsConvertValue     aWorld aSI cState vp vt
    (CRSConvertValueBy    vpA vpB)             -> crsConvertValueBy   aWorld aSI cState vpA vpB
    (CRSConvertValueWith  vpA vpB)             -> crsConvertValueWith aWorld aSI cState vpA vpB
    (CRSRandom            vp  vt)              -> crsRandom           aWorld aSI cState vp vt
    (CRSRandomBy          vpA vpB)             -> crsRandomBy         aWorld aSI cState vpA vpB
    (CRSRandomWith        vpA vt vpC vpD vpE)  -> crsRandomWith       aWorld aSI cState vpA vt vpC vpD vpE
    (CRSRandomWithBy      vpA vpB vpC vpD vpE) -> crsRandomWithBy     aWorld aSI cState vpA vpB vpC vpD vpE
    (CRSElapseTime        vpA vpB)             -> crsElapsedTime      aWorld aSI cState vpA vpB
    (CRSSPControl         vp)                  -> crsSPControl        aWorld aSI cState vp
    (CRSSIControl         vpA vpB)             -> crsSIControl        aWorld aSI cState vpA vpB
    (CRSSIInit            vpA vpB vpC)         -> crsSIInit           aWorld aSI cState vpA vpB vpC
    _ -> error "[ERROR]<runInstruction :=: otherwise> Can't be reached"
