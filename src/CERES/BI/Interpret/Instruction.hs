module CERES.BI.Interpret.Instruction where


import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe

import           System.Random.SplitMix

import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.Value.Method

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Cache
import           CERES.BI.Interpret.Spool

import           Debug


crsInitVariable :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsInitVariable     World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsSetValue :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsSetValue         World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsDeleteVariable :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsDeleteVariable   World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vp = undefined

crsModifyValue :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> CERESOperator -> (WorldCache, LocalVariables, LocalCache, RG)
crsModifyValue      World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB cOp = undefined

crsCopyValue :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsCopyValue        World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsConvertValue :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> ValueType -> (WorldCache, LocalVariables, LocalCache, RG)
crsConvertValue     World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vp vt = undefined

crsConvertValueBy :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsConvertValueBy   World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsConvertValueWith :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsConvertValueWith World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsRandom :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsRandom           World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsElapsedTime :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsElapsedTime      World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined
 where
  -- NOTE: 1. Estimate next instruction's executing internal time
  -- NOTE: 2. If elapsedTime + executingTime > worldTSSize, then store executingTime in localVariables and end interpreting up
  -- NOTE: 3. Else, do interpret, and modify (+executingTime) elapsedTime
  -- NOTE: Calculate executingTime not runCEREScript but here, because length of executingTime is depends on an instruction
  (executingTime, rg1) = first (`rem` worldTSSize) . nextInt $ rg
  elapsedTime          = maybe 0 getInt . IM.lookup elapsedInternalTimeID $ localVars
  doSkip               = elapsedTime + executingTime > worldTSSize
  elapsedInternalTime  = maybe False getBool $ IM.lookup resumeCodeID localVars

crsSPControl :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsSPControl        World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vp = undefined

crsSIControl :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsSIControl        World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB = undefined

crsSIInit :: World -> SpoolInstance -> (WorldCache, LocalVariables, LocalCache, RG) -> VPosition -> VPosition -> VPosition -> (WorldCache, LocalVariables, LocalCache, RG)
crsSIInit           World {..} SI {..} cState@(wc@(hCache, dCache, vCache), localVars, localCache, rg) vpA vpB vpC = undefined