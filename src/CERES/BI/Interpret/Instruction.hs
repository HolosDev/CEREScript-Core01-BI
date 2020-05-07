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


crsInitVariable :: Input -> VPosition -> VPosition -> Env
crsInitVariable anInput@(_, _, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsInitVariableAt :: Input -> VPosition -> VPosition -> Env
crsInitVariableAt anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsSetValue :: Input -> VPosition -> VPosition -> Env
crsSetValue anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsDeleteVariable :: Input -> VPosition -> Env
crsDeleteVariable anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp
  = undefined

crsModifyValue1 :: Input -> VPosition -> CERESOperator -> Env
crsModifyValue1 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp cOp
  = undefined

crsModifyValue2 :: Input -> VPosition -> VPosition -> CERESOperator -> Env
crsModifyValue2 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB cOp
  = undefined

crsModifyValue3
  :: Input -> VPosition -> VPosition -> CERESOperator -> VPosition -> Env
crsModifyValue3 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB cOp vpC
  = undefined

crsCopyValue :: Input -> VPosition -> VPosition -> Env
crsCopyValue anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsConvertValue :: Input -> VPosition -> ValueType -> Env
crsConvertValue anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp vt
  = undefined

crsConvertValueBy :: Input -> VPosition -> VPosition -> Env
crsConvertValueBy anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsConvertValueWith :: Input -> VPosition -> VPosition -> Env
crsConvertValueWith anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsReplaceText :: Input -> VPosition -> Env
crsReplaceText anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp
  = undefined

crsReplaceTextTo :: Input -> VPosition -> VPosition -> Env
crsReplaceTextTo anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsGetVPosition :: Input -> VPosition -> VPosition -> Env
crsGetVPosition anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  targetStr     = getValue anInput vpA
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

crsSetVPosition :: Input -> VPosition -> VPosition -> Env
crsSetVPosition anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsRandom :: Input -> VPosition -> ValueType -> Env
crsRandom (aWorld, _, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp vType
  = setEnv aWorld vp W (Just rValue) (wc, lc, tCache, nextRG)
  where (rValue, nextRG) = randomValue vType rg

crsRandomBy :: Input -> VPosition -> VPosition -> Env
crsRandomBy anInput vpA vpB = crsRandom anInput vpA vt
  where vt = getValueType $ getValue anInput vpB

crsRandomWith
  :: Input
  -> VPosition
  -> ValueType
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsRandomWith anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vType vpC vpD vpE
  = notYetImpl "crsRandomWith"

crsRandomWithBy
  :: Input
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsRandomWithBy anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB vpC vpD vpE
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

crsElapsedTime :: Input -> VPosition -> VPosition -> Env
crsElapsedTime anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
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

crsSPControl :: Input -> VPosition -> Env
crsSPControl anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp
  = undefined

crsSIControl :: Input -> VPosition -> VPosition -> Env
crsSIControl anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

-- NOTE: crsSIInit does not initiate SI by itself, but would be done by `runSpoolInstance`
crsSIInit :: Input -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsSIInit anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB vpC vpD
  = undefined
  where spoolID = undefined

crsSIEnd :: Input -> VPosition -> Env
crsSIEnd anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vp
  = undefined

crsNoop :: Input -> Env
crsNoop anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg))
  = undefined

crsLog :: Input -> VPosition -> VPosition -> Env
crsLog anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsParseScript :: Input -> VPosition -> VPosition -> Env
crsParseScript anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) vpA vpB
  = undefined

crsToInterpreter0 :: Input -> CHeader -> Env
crsToInterpreter0 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader
  = undefined

crsToInterpreter1 :: Input -> CHeader -> VPosition -> Env
crsToInterpreter1 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA
  = undefined

crsToInterpreter2 :: Input -> CHeader -> VPosition -> VPosition -> Env
crsToInterpreter2 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB
  = undefined

crsToInterpreter3
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> Env
crsToInterpreter3 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC
  = undefined

crsToInterpreter4
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsToInterpreter4 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD
  = undefined

crsToInterpreter5
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter5 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE
  = undefined

crsToInterpreter6
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter6 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF
  = undefined

crsToInterpreter7
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsToInterpreter7 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF vpG
  = undefined

crsToInterpreter8
  :: Input
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
crsToInterpreter8 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined


crsExtend0 :: Input -> CHeader -> Env
crsExtend0 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader
  = undefined

crsExtend1 :: Input -> CHeader -> VPosition -> Env
crsExtend1 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA
  = undefined

crsExtend2 :: Input -> CHeader -> VPosition -> VPosition -> Env
crsExtend2 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB
  = undefined

crsExtend3 :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> Env
crsExtend3 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC
  = undefined

crsExtend4
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsExtend4 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD
  = undefined

crsExtend5
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend5 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE
  = undefined

crsExtend6
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend6 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF
  = undefined

crsExtend7
  :: Input
  -> CHeader
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsExtend7 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF vpG
  = undefined

crsExtend8
  :: Input
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
crsExtend8 anInput@(aWorld@World {..}, aSI@SI {..}, cState@(wc@(hCache, nHCache, vCache, nVCache, dCache, nDCache), lc@(lVCache, lNVCache, lTCache, lNTCache), tCache, rg)) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined
