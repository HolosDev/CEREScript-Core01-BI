module CERES.BI.Interpret.Instruction where


import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           TextShow


import           CERES.Operate

import           Data.CERES.Data
import           Data.CERES.Data.Method
import           Data.CERES.Operator
import           Data.CERES.Parser
import           Data.CERES.Type
import           Data.CERES.Variable.Parser

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
crsInitVariable World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsInitVariableAt
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsInitVariableAt World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsSetValue :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsSetValue World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsDeleteVariable :: World -> SpoolInstance -> Env -> VPosition -> Env
crsDeleteVariable World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp
  = undefined

crsModifyValue1
  :: World -> SpoolInstance -> Env -> VPosition -> CERESOperator -> Env
crsModifyValue1 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp cOp
  = undefined

crsModifyValue2
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> VPosition
  -> CERESOperator
  -> Env
crsModifyValue2 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB cOp
  = undefined

crsModifyValue3
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> VPosition
  -> CERESOperator
  -> VPosition
  -> Env
crsModifyValue3 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB cOp vpC
  = undefined

crsCopyValue :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsCopyValue World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsConvertValue
  :: World -> SpoolInstance -> Env -> VPosition -> ValueType -> Env
crsConvertValue World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp vt
  = undefined

crsConvertValueBy
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsConvertValueBy World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsConvertValueWith
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsConvertValueWith World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsReplaceText :: World -> SpoolInstance -> Env -> VPosition -> Env
crsReplaceText World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp
  = undefined

crsReplaceTextTo
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsReplaceTextTo World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsGetVPosition
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsGetVPosition aWorld aSI cState vpA vpB = newCState
 where
  targetStr     = getEnv aWorld cState vpA
  lazyTargetStr = TL.fromStrict . getStr $ targetStr
  rTargetPtr    = parseVariablePosition lazyTargetStr
  mTargetPtr    = getResult rTargetPtr
  aMsg          = fromJust . getMessage $ rTargetPtr
  newCState     = maybe
    ( setEnv
        aWorld
        (VP AtTricky (VIN "DebugLog"))
        W
        (Just . StrValue $ T.append "Fail to read VariablePosition: "
                                    (showt targetStr)
        )
    . setEnv aWorld vpB W (Just . ErrValue $ aMsg)
    $ cState
    )
    (\p -> setEnv aWorld vpB W (Just . PtrValue $ p) cState)
    mTargetPtr

crsSetVPosition
  :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsSetVPosition World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsRandom :: World -> SpoolInstance -> Env -> VPosition -> ValueType -> Env
crsRandom aWorld SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp vType
  = setEnv aWorld
           vp
           W
           (Just rValue)
           (wc, (lVCache, lNVCache, lTCache, lNTCache), tCache, nextRG)
  where (rValue, nextRG) = randomValue vType rg

crsRandomBy :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsRandomBy aWorld aSI cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = crsRandom aWorld aSI cState vpA vt
  where vt = getValueType . getEnv aWorld cState $ vpB

crsRandomWith
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> ValueType
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsRandomWith World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vType vpC vpD vpE
  = notYetImpl "crsRandomWith"

crsRandomWithBy
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsRandomWithBy World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB vpC vpD vpE
  = notYetImpl "crsRandomWith"

randomValueBy :: Value -> RG -> (Value, RG)
randomValueBy v = randomValue (getValueType v)

randomValue :: ValueType -> RG -> (Value, RG)
randomValue vType rg = case vType of
  VTInt  -> first IntValue . nextInt $ rg
  VTDbl  -> first DblValue . nextDouble $ rg
  VTBool -> first (BoolValue . odd) . nextWord64 $ rg
  VTAtom -> (AtomValue, rg)
  VTStr ->
    (ErrValue "[ERROR]<crsRandom :=: VTStr> Not proper value type for RNG", rg)
  VTArr ->
    (ErrValue "[ERROR]<crsRandom :=: VTArr> Not proper value type for RNG", rg)
  VTPtr ->
    (ErrValue "[ERROR]<crsRandom :=: VTPtr> Not proper value type for RNG", rg)
  VTScr ->
    (ErrValue "[ERROR]<crsRandom :=: VTScr> Not proper value type for RNG", rg)
  VTErr ->
    (ErrValue "[ERROR]<crsRandom :=: VTErr> Not proper value type for RNG", rg)
  _ -> error "[ERROR]<crsRandom> Can't be reached"

crsElapsedTime :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsElapsedTime World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined
 where
  -- NOTE: 1. Estimate next instruction's executing internal time
  -- NOTE: 2. If elapsedTime + executingTime > worldTSSize, then store executingTime in localVariables and end interpreting up
  -- NOTE: 3. Else, do interpret, and modify (+executingTime) elapsedTime
  -- NOTE: Calculate executingTime not runCEREScript but here, because length of executingTime is depends on an instruction
  (executingTime, rg1) = bmwrInt worldTSSize rg
  elapsedTime = maybe 0 getInt . IM.lookup elapsedInternalTimeIdx $ lVCache
  newElapsedTime       = elapsedTime + executingTime
  doSkip               = newElapsedTime > worldTSSize
  elapsedInternalTime  = maybe False getBool $ IM.lookup resumeCodeIdx lVCache

crsSPControl :: World -> SpoolInstance -> Env -> VPosition -> Env
crsSPControl World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp
  = undefined

crsSIControl :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsSIControl World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

-- NOTE: crsSIInit does not initiate SI by itself, but would be done by `runSpoolInstance`
crsSIInit
  :: World
  -> SpoolInstance
  -> Env
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsSIInit World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB vpC vpD
  = undefined
  where spoolID = undefined

crsSIEnd :: World -> SpoolInstance -> Env -> VPosition -> Env
crsSIEnd World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vp
  = undefined

crsNoop :: World -> SpoolInstance -> Env -> Env
crsNoop World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg)
  = undefined

crsLog :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsLog World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsParseScript :: World -> SpoolInstance -> Env -> VPosition -> VPosition -> Env
crsParseScript World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) vpA vpB
  = undefined

crsToInterpreter0 :: World -> SpoolInstance -> Env -> CHeader -> Env
crsToInterpreter0 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader
  = undefined

crsToInterpreter1
  :: World -> SpoolInstance -> Env -> CHeader -> VPosition -> Env
crsToInterpreter1 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA
  = undefined

crsToInterpreter2
  :: World -> SpoolInstance -> Env -> CHeader -> VPosition -> VPosition -> Env
crsToInterpreter2 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB
  = undefined

crsToInterpreter3
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter3 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC
  = undefined

crsToInterpreter4
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter4 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD
  = undefined

crsToInterpreter5
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter5 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE
  = undefined

crsToInterpreter6
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter6 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF
  = undefined

crsToInterpreter7
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter7 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF vpG
  = undefined

crsToInterpreter8
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter8 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined


crsExtend0 :: World -> SpoolInstance -> Env -> CHeader -> Env
crsExtend0 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader
  = undefined

crsExtend1 :: World -> SpoolInstance -> Env -> CHeader -> VPosition -> Env
crsExtend1 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA
  = undefined

crsExtend2
  :: World -> SpoolInstance -> Env -> CHeader -> VPosition -> VPosition -> Env
crsExtend2 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB
  = undefined

crsExtend3
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend3 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC
  = undefined

crsExtend4
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend4 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD
  = undefined

crsExtend5
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend5 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE
  = undefined

crsExtend6
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend6 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF
  = undefined

crsExtend7
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend7 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF vpG
  = undefined

crsExtend8
  :: World
  -> SpoolInstance
  -> Env
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend8 World {..} SI {..} cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), (lVCache, lNVCache, lTCache, lNTCache), tCache, rg) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined
