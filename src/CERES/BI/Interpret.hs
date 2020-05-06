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
  (newCache@(newWorldCache, (newLVCache, newLNVCache, newLTCache, newLNTCache), newTrickCache, newRG), restCEREScript)
    = runCEREScript world si (wCache, iLocalCache, blankVNM, siRG) siRestScript
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
          (\s -> runMaker (csScript s) (world, newCache))
        $ IM.lookup siSpoolID worldSpools
  newSI = si { siLocalVars  = nextLocalVars
             , siLocalNVars = nextLocalNVars
             , siRestScript = nextCEREScript
             , siRG         = newRG
             }


runCEREScript
  :: World -> SpoolInstance -> Env -> CEREScript -> (Env, CEREScript)
runCEREScript aWorld@World {..} aSI@SI {..} = runCEREScriptSub
 where
  runCEREScriptSub cState [] = (cState, [])
  runCEREScriptSub cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg) (ceres : cScript)
    = if sp
      then ((nextWC, nextLC, nextTCache, nextRG), nextCEREScript)
      else runCEREScriptSub (nextWC, nextLC, nextTCache, nextRG) nextCEREScript
    -- NOTE: si == True, then end runCEREScript
   where
    (newWC, newLC, newTCache, newRG) = runInstruction aWorld aSI cState ceres
    -- TODO: Check Stop or Pause
    spCode = maybe "" getStr $ IM.lookup spCodeIdx newLTCache
    sp                               = spCode == "Stop" || spCode == "Pause"
    retentionCode                    = case spCode of
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
    nextTCache     = newTCache
    nextRG         = newRG


runInstruction :: World -> SpoolInstance -> Env -> CERES -> Env
runInstruction aWorld aSI cState aCERES = case aCERES of
  (CRSInitVariable   vpA vpB) -> crsInitVariable aWorld aSI cState vpA vpB
  (CRSInitVariableAt vpA vpB) -> crsInitVariableAt aWorld aSI cState vpA vpB
  (CRSSetValue       vpA vpB) -> crsSetValue aWorld aSI cState vpA vpB
  (CRSDeleteVariable vp     ) -> crsDeleteVariable aWorld aSI cState vp
  (CRSModifyValue1 vpA cOp  ) -> crsModifyValue1 aWorld aSI cState vpA cOp
  (CRSModifyValue2 vpA vpB cOp) ->
    crsModifyValue2 aWorld aSI cState vpA vpB cOp
  (CRSModifyValue3 vpA vpB cOp vpC) ->
    crsModifyValue3 aWorld aSI cState vpA vpB cOp vpC
  (CRSCopyValue      vpA vpB) -> crsCopyValue aWorld aSI cState vpA vpB
  (CRSConvertValue   vp  vt ) -> crsConvertValue aWorld aSI cState vp vt
  (CRSConvertValueBy vpA vpB) -> crsConvertValueBy aWorld aSI cState vpA vpB
  (CRSConvertValueWith vpA vpB) ->
    crsConvertValueWith aWorld aSI cState vpA vpB
  (CRSReplaceText vp       ) -> crsReplaceText aWorld aSI cState vp
  (CRSReplaceTextTo vpA vpB) -> crsReplaceTextTo aWorld aSI cState vpA vpB
  (CRSGetVPosition  vpA vpB) -> crsGetVPosition aWorld aSI cState vpA vpB
  (CRSSetVPosition  vpA vpB) -> crsSetVPosition aWorld aSI cState vpA vpB
  (CRSRandom        vp  vt ) -> crsRandom aWorld aSI cState vp vt
  (CRSRandomBy      vpA vpB) -> crsRandomBy aWorld aSI cState vpA vpB
  (CRSRandomWith vpA vt vpC vpD vpE) ->
    crsRandomWith aWorld aSI cState vpA vt vpC vpD vpE
  (CRSRandomWithBy vpA vpB vpC vpD vpE) ->
    crsRandomWithBy aWorld aSI cState vpA vpB vpC vpD vpE
  (CRSElapseTime vpA vpB    ) -> crsElapsedTime aWorld aSI cState vpA vpB
  (CRSSPControl vp          ) -> crsSPControl aWorld aSI cState vp
  (CRSSIControl vpA vpB     ) -> crsSIControl aWorld aSI cState vpA vpB
  (CRSSIInit vpA vpB vpC vpD) -> crsSIInit aWorld aSI cState vpA vpB vpC vpD
  (CRSSIEnd vp              ) -> crsSIEnd aWorld aSI cState vp
  CRSNoop                     -> crsNoop aWorld aSI cState
  (CRSLog         vpA vpB   ) -> crsLog aWorld aSI cState vpA vpB
  (CRSParseScript vpA vpB   ) -> crsParseScript aWorld aSI cState vpA vpB
  (CRSToInterpreter0 iHeader) -> crsToInterpreter0 aWorld aSI cState iHeader
  (CRSToInterpreter1 iHeader vpA) ->
    crsToInterpreter1 aWorld aSI cState iHeader vpA
  (CRSToInterpreter2 iHeader vpA vpB) ->
    crsToInterpreter2 aWorld aSI cState iHeader vpA vpB
  (CRSToInterpreter3 iHeader vpA vpB vpC) ->
    crsToInterpreter3 aWorld aSI cState iHeader vpA vpB vpC
  (CRSToInterpreter4 iHeader vpA vpB vpC vpD) ->
    crsToInterpreter4 aWorld aSI cState iHeader vpA vpB vpC vpD
  (CRSToInterpreter5 iHeader vpA vpB vpC vpD vpE) ->
    crsToInterpreter5 aWorld aSI cState iHeader vpA vpB vpC vpD vpE
  (CRSToInterpreter6 iHeader vpA vpB vpC vpD vpE vpF) ->
    crsToInterpreter6 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF
  (CRSToInterpreter7 iHeader vpA vpB vpC vpD vpE vpF vpG) ->
    crsToInterpreter7 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF vpG
  (CRSToInterpreter8 iHeader vpA vpB vpC vpD vpE vpF vpG vpH) ->
    crsToInterpreter8 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  (CRSExtend0 iHeader        ) -> crsExtend0 aWorld aSI cState iHeader
  (CRSExtend1 iHeader vpA    ) -> crsExtend1 aWorld aSI cState iHeader vpA
  (CRSExtend2 iHeader vpA vpB) -> crsExtend2 aWorld aSI cState iHeader vpA vpB
  (CRSExtend3 iHeader vpA vpB vpC) ->
    crsExtend3 aWorld aSI cState iHeader vpA vpB vpC
  (CRSExtend4 iHeader vpA vpB vpC vpD) ->
    crsExtend4 aWorld aSI cState iHeader vpA vpB vpC vpD
  (CRSExtend5 iHeader vpA vpB vpC vpD vpE) ->
    crsExtend5 aWorld aSI cState iHeader vpA vpB vpC vpD vpE
  (CRSExtend6 iHeader vpA vpB vpC vpD vpE vpF) ->
    crsExtend6 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF
  (CRSExtend7 iHeader vpA vpB vpC vpD vpE vpF vpG) ->
    crsExtend7 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF vpG
  (CRSExtend8 iHeader vpA vpB vpC vpD vpE vpF vpG vpH) ->
    crsExtend8 aWorld aSI cState iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  _ -> error "[ERROR]<runInstruction :=: otherwise> Can't be reached"
