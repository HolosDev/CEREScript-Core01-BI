module CERES.BI.Interpret where


import           Control.Parallel
import           Control.Parallel.Strategies

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
import           CERES.BI.Interpret.Script
import           CERES.BI.Interpret.SpoolTree

import           CERES.BI.Type

import           CERES.BI.Util

import           Debug

import           Util.Parallel


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
  resultList       = parMap rpar (runSpoolTree aWorld) aSpoolForest
  siStatusList     = concatMap fst resultList
  wcList           = map snd resultList
  -- TODO: Change cacheCommitter style or union WorldCache in wcList
  committed        = foldr worldCacheCommitter worldState wcList
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
  newSITable    = siisExecutor worldTime worldSITable siStatusList
  newWorld =
    updateWorldInTimeSlot aWorld newWorldState newSITable nextWorldTime


{-
# How to initiate SpoolInstance
`runInstruction` or `runCEREScript` can't initiate a SpoolInstance, because they returns only Env and/or CEREScript.
Therefore, `runSpoolInstance` should initiate a new SpoolInstance.

To make it, `runCEREScript` should end its recursive call when `crsSIInit` run without reaching `CRSJump` or etc.
-}

-- TODO: How to avoid SpoolInstance ID collision
runSpoolTree :: World -> SpoolTree -> ([SIStatus], WorldCache)
runSpoolTree aWorld@World {..} aSpoolTree@SpoolTree {..} =
  (siisList, newWorldCache)
 where
  worldCache                = worldCacheMaker aSpoolTree aWorld
  (siisList, newWorldCache) = foldr
    (\s (l, c) -> joint l (runSpoolInstance aWorld s c))
    ([], worldCache)
    siList
  joint aList (s, aWorldCache) = (s : aList, aWorldCache)

runSpoolInstance
  :: World -> SpoolInstance -> WorldCache -> (SIStatus, WorldCache)
runSpoolInstance world@World {..} si@SI {..} aWCache =
  ((SIParams { siIS = siis }, newSI), wCache newCache)
 where
  -- TODO: Change `StrValue "Retain"` as a named constant
  isResume = maybe False getBool $ vMapLookup resumeCodeIdx siLocalVars
  -- TODO: Not sure to initialize ExecutingTime variable
  -- FIXME: Should have control with prior retain with Maker
  iLVars =
    (if isResume then id else IM.insert executingTimeIdx (IntValue 0))
      . IM.insert resumeCodeIdx (BoolValue False)
      $ siLocalVars
  iLNVars                    = siLocalNVars
  iLTemp                     = csInitLocalTemp $ worldSpools IM.! siSpoolID
  iLNTemp                    = siLocalNTemp
  iLocalCache                = LocalCache iLVars iLNVars iLTemp iLNTemp
  (newCache, restCEREScript) = runCEREScript
    (world, si, Env aWCache iLocalCache blankVNM siRG)
    siRestScript
  --initSI = getBool $ vMapLookup initSICodeIdx newLTCache
  newLC    = lCache newCache
  siisCode = maybe "Retain" getStr $ vMapLookup retainCodeIdx (lTemp newLC)
  (doAbolish, doInit, nextLocalVars, nextLocalNVars) = case siisCode of
    "Retain"  -> (False, False, lVars newLC, lNVars newLC)
    "Forget"  -> (False, False, blankVM, blankVNM)
    -- FIXME: Add SI initiation logic
    "Init"    -> (False, True, blankVM, blankVNM)
    "Abolish" -> (True, False, blankVM, blankVNM)
    _ -> error "[ERROR]<runSpoolInstance :=: _> Undefined Retention Code"
  -- NOTE: SIJump takes relative time-slot
  jumpTarget = maybe 1 getInt $ vMapLookup jumpOffsetIdx (lTemp newLC)
  siis =
    if doAbolish || isScriptEnd restCEREScript then SIEnd else SIJump jumpTarget
  nextCEREScript = if doInit then newCEREScript else restCEREScript
   where
    newCEREScript =
      maybe
          (  error
          $  "[ERROR]<runSpoolInstance :=: newCEREScript> No such SpoolID("
          <> show siSpoolID
          <> ") in worldSpools"
          )
          (\s -> runMaker (csScript s) (world, newCache))
        $ IM.lookup siSpoolID worldSpools
  newSI = si { siLocalVars  = nextLocalVars
             , siLocalNVars = nextLocalNVars
             , siRestScript = nextCEREScript
             , siRG         = rg newCache
             }


runCEREScript :: Input -> CEREScript -> (Env, CEREScript)
runCEREScript (_, _, cState) [] = (cState, [])
runCEREScript anInput@(aWorld@World {..}, aSI@SI {..}, cState) (ceres : cScript)
  = logConsole $ if sp -- NOTE: sp == True, then end runCEREScript
    then (Env nextWC nextLC nextTC nextRG, nextCEREScript)
    else runCEREScript (aWorld, aSI, Env nextWC nextLC nextTC nextRG)
                       nextCEREScript
 where
  newCState       = runInstruction (aWorld, aSI, cState) ceres
  newInput        = (aWorld, aSI, newCState)
  endOfScript     = isScriptEnd cScript
  doesNotExceedTS = not $ exceedTS newInput
  -- TODO: Check Stop or Pause
  spCode          = maybe "" getStr $ vMapLookup spCodeIdx (lTemp newLC)
  sp              = spCode == "Stop" || spCode == "Pause"
  retentionCode   = case spCode of
    "Stop"  -> "Abolish"
    -- TODO: Not sure do I need to identify whether this is "Pause"
    "Pause" -> "Retain"
    _       -> maybe "Retain" getStr $ vMapLookup retainCodeIdx (lTemp newLC)
  -- TODO: Check the instruction is executed or not
  resumeFlag     = maybe False getBool $ vMapLookup resumeCodeIdx (lVars newLC)
  nextCEREScript = if resumeFlag then (ceres : cScript) else cScript
  newLC          = lCache newCState
  newTC          = tCache newCState
  logMsg         = undefined
  logConsole     = undefined
  nextWC         = wCache newCState
  nextLVars = vMapInsert retainCodeIdx (StrValue retentionCode) (lVars newLC)
  nextLNVars     = lNVars newLC
  nextLTemp      = lTemp newLC
  nextLNTemp     = lNTemp newLC
  nextLC         = LocalCache nextLVars nextLNVars nextLTemp nextLNTemp
  nextTC         = vNMapDeleteSubmap consoleLogKey newTC
  nextRG         = rg newCState


runInstruction :: Input -> CERES -> Env
runInstruction anInput aCERES = case aCERES of
  (CRSInitVariable   vpA vpB      ) -> crsInitVariable anInput vpA vpB
  (CRSInitVariableAt vpA vpB      ) -> crsInitVariableAt anInput vpA vpB
  (CRSSetValue       vpA vpB      ) -> crsSetValue anInput vpA vpB
  (CRSDeleteVariable vp           ) -> crsDeleteVariable anInput vp
  (CRSModifyValue1 vpA cOp        ) -> crsModifyValue1 anInput vpA cOp
  (CRSModifyValue2 vpA vpB cOp    ) -> crsModifyValue2 anInput vpA vpB cOp
  (CRSModifyValue3 vpA vpB cOp vpC) -> crsModifyValue3 anInput vpA vpB cOp vpC
  (CRSCopyValue        vpA vpB    ) -> crsCopyValue anInput vpA vpB
  (CRSConvertValue     vp  vt     ) -> crsConvertValue anInput vp vt
  (CRSConvertValueBy   vpA vpB    ) -> crsConvertValueBy anInput vpA vpB
  (CRSConvertValueWith vpA vpB    ) -> crsConvertValueWith anInput vpA vpB
  (CRSReplaceText vp              ) -> crsReplaceText anInput vp
  (CRSReplaceTextTo vpA vpB       ) -> crsReplaceTextTo anInput vpA vpB
  (CRSGetVPosition  vpA vpB       ) -> crsGetVPosition anInput vpA vpB
  (CRSSetVPosition  vpA vpB       ) -> crsSetVPosition anInput vpA vpB
  (CRSRandom        vp  vt        ) -> crsRandom anInput vp vt
  (CRSRandomBy      vpA vpB       ) -> crsRandomBy anInput vpA vpB
  (CRSRandomWith vpA vt vpC vpD vpE) ->
    crsRandomWith anInput vpA vt vpC vpD vpE
  (CRSRandomWithBy vpA vpB vpC vpD vpE) ->
    crsRandomWithBy anInput vpA vpB vpC vpD vpE
  (CRSElapseTime vpA vpB    )     -> crsElapsedTime anInput vpA vpB
  (CRSSPControl vp          )     -> crsSPControl anInput vp
  (CRSSIControl vpA vpB     )     -> crsSIControl anInput vpA vpB
  (CRSSIInit vpA vpB vpC vpD)     -> crsSIInit anInput vpA vpB vpC vpD
  (CRSSIEnd vp              )     -> crsSIEnd anInput vp
  CRSNoop                         -> crsNoop anInput
  (CRSLog         vpA vpB       ) -> crsLog anInput vpA vpB
  (CRSParseScript vpA vpB       ) -> crsParseScript anInput vpA vpB
  (CRSToInterpreter0 iHeader    ) -> crsToInterpreter0 anInput iHeader
  (CRSToInterpreter1 iHeader vpA) -> crsToInterpreter1 anInput iHeader vpA
  (CRSToInterpreter2 iHeader vpA vpB) ->
    crsToInterpreter2 anInput iHeader vpA vpB
  (CRSToInterpreter3 iHeader vpA vpB vpC) ->
    crsToInterpreter3 anInput iHeader vpA vpB vpC
  (CRSToInterpreter4 iHeader vpA vpB vpC vpD) ->
    crsToInterpreter4 anInput iHeader vpA vpB vpC vpD
  (CRSToInterpreter5 iHeader vpA vpB vpC vpD vpE) ->
    crsToInterpreter5 anInput iHeader vpA vpB vpC vpD vpE
  (CRSToInterpreter6 iHeader vpA vpB vpC vpD vpE vpF) ->
    crsToInterpreter6 anInput iHeader vpA vpB vpC vpD vpE vpF
  (CRSToInterpreter7 iHeader vpA vpB vpC vpD vpE vpF vpG) ->
    crsToInterpreter7 anInput iHeader vpA vpB vpC vpD vpE vpF vpG
  (CRSToInterpreter8 iHeader vpA vpB vpC vpD vpE vpF vpG vpH) ->
    crsToInterpreter8 anInput iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  (CRSExtend0 iHeader            ) -> crsExtend0 anInput iHeader
  (CRSExtend1 iHeader vpA        ) -> crsExtend1 anInput iHeader vpA
  (CRSExtend2 iHeader vpA vpB    ) -> crsExtend2 anInput iHeader vpA vpB
  (CRSExtend3 iHeader vpA vpB vpC) -> crsExtend3 anInput iHeader vpA vpB vpC
  (CRSExtend4 iHeader vpA vpB vpC vpD) ->
    crsExtend4 anInput iHeader vpA vpB vpC vpD
  (CRSExtend5 iHeader vpA vpB vpC vpD vpE) ->
    crsExtend5 anInput iHeader vpA vpB vpC vpD vpE
  (CRSExtend6 iHeader vpA vpB vpC vpD vpE vpF) ->
    crsExtend6 anInput iHeader vpA vpB vpC vpD vpE vpF
  (CRSExtend7 iHeader vpA vpB vpC vpD vpE vpF vpG) ->
    crsExtend7 anInput iHeader vpA vpB vpC vpD vpE vpF vpG
  (CRSExtend8 iHeader vpA vpB vpC vpD vpE vpF vpG vpH) ->
    crsExtend8 anInput iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  _ -> error "[ERROR]<runInstruction :=: otherwise> Can't be reached"
