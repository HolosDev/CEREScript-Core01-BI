module CERES.BI.Interpret.Instruction where


import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe


import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.Value.Method

import           CERES.BI.Data
import           CERES.BI.Data.Cache.Function
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Cache
import           CERES.BI.Interpret.Spool

import           CERES.BI.Type

import           CERES.BI.Util.Random

import           Debug


crsInitVariable
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsInitVariable World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsInitVariableAt
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsInitVariableAt World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsSetValue :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsSetValue World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsDeleteVariable :: World -> SpoolInstance -> Env -> VPosition -> Env
crsDeleteVariable World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vp
  = undefined

crsModifyValue
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> VPosition
  -> CERESOperator
  -> Env
crsModifyValue World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB cOp
  = undefined

crsCopyValue :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsCopyValue World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsConvertValue
  :: World -> SpoolInstance -> Env -> VPosition -> ValueType -> Env
crsConvertValue World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vp vt
  = undefined

crsConvertValueBy
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsConvertValueBy World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsConvertValueWith
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsConvertValueWith World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined

crsRandom :: World -> SpoolInstance -> Env -> VPosition -> ValueType -> Env
crsRandom aWorld SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vp vType
  = setEnv aWorld vp W (Just rValue) (wc, localVars, localCache, nextRG)
 where
  (rValue, nextRG) = randomValue vType rg

crsRandomBy :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsRandomBy aWorld aSI cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = crsRandom aWorld aSI cState vpA vt
 where
  vt = getValueType . getEnv aWorld vpB $ cState

crsRandomWith :: World -> SpoolInstance -> Env -> VPosition -> ValueType -> VPosition -> VPosition -> VPosition -> Env
crsRandomWith World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vType vpC vpD vpE
  = notYetImpl "crsRandomWith"

crsRandomWithBy :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsRandomWithBy World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB vpC vpD vpE
  = notYetImpl "crsRandomWith"

randomValueBy :: Value -> RG -> (Value, RG)
randomValueBy v = randomValue (getValueType v)

randomValue :: ValueType -> RG -> (Value, RG)
randomValue vType rg = case vType of
    VTInt -> first IntValue . nextInt $ rg
    VTDbl -> first DblValue . nextDouble $ rg
    VTBool -> first (BoolValue . odd) . nextWord64 $ rg
    VTAtom -> (AtomValue, rg)
    VTStr -> (ErrValue "[ERROR]<crsRandom :=: VTStr> Not proper value type for RNG", rg)
    VTErr -> (ErrValue "[ERROR]<crsRandom :=: VTErr> Not proper value type for RNG", rg)

crsElapsedTime :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsElapsedTime World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined
 where
  -- NOTE: 1. Estimate next instruction's executing internal time
  -- NOTE: 2. If elapsedTime + executingTime > worldTSSize, then store executingTime in localVariables and end interpreting up
  -- NOTE: 3. Else, do interpret, and modify (+executingTime) elapsedTime
  -- NOTE: Calculate executingTime not runCEREScript but here, because length of executingTime is depends on an instruction
  (executingTime, rg1) = bmwrInt worldTSSize rg
  elapsedTime = maybe 0 getInt . IM.lookup elapsedInternalTimeIdx $ localVars
  newElapsedTime = elapsedTime + executingTime
  doSkip               = newElapsedTime > worldTSSize
  elapsedInternalTime  = maybe False getBool $ IM.lookup resumeCodeIdx localVars

crsSPControl :: World -> SpoolInstance -> Env -> VPosition -> Env
crsSPControl World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vp
  = undefined

crsSIControl :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsSIControl World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB
  = undefined


-- NOTE: crsSIInit does not initiate SI by itself, but would be done by `runSpoolInstance`
crsSIInit
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> VPosition -> Env
crsSIInit World {..} SI {..} cState@(wc@(hCache, dCache, nCache, vCache), localVars, localCache, rg) vpA vpB vpC
  = undefined
 where
  spoolID = undefined
