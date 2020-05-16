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
import           CERES.BI.Interpret.Modify
import           CERES.BI.Interpret.Spool

import           CERES.BI.Type

import           CERES.BI.Util
import           CERES.BI.Util.Random

import           Debug
import           Debug.Trace


dLogAndErr :: Input -> Message -> VPosition -> Message -> Env
dLogAndErr anInput@(aWorld, _, cState) logMsg vpT errMsg =
  setEnv aWorld (VP AtTricky (VIN "DebugLog")) W (Just . StrValue $ logMsg)
    . setEnv aWorld vpT W (Just . ErrValue $ errMsg)
    $ cState

crsInitVariable :: Input -> VPosition -> VPosition -> Env
crsInitVariable anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  theValue     = getValue anInput vpB
  theExistence = getMValue anInput vpA
  theDLogMsg =
    T.append "[Fail]<crsInitVariable> VP exists already at" (showt vpA)
  newCState = maybe
    (setEnv aWorld vpA W (Just theValue) cState)
    (\_ -> dLogAndErr anInput theDLogMsg vpA "[Fail]<InitVariable>")
    theExistence

crsInitVariableAt :: Input -> VPosition -> VPosition -> Env
crsInitVariableAt _ _ _ =
  error "[ERROR]<crsInitVariableAt> Would be deprecated"

-- TODO: Do I need to check existence?
crsSetValue :: Input -> VPosition -> VPosition -> Env
crsSetValue anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  theValue     = getValue anInput vpB
  theExistence = getMValue anInput vpA
  theDLogMsg =
    T.append "[Fail]<crsSetVariable> A variable does not exists at" (showt vpA)
  newCState = maybe (dLogAndErr anInput theDLogMsg vpA "[Fail]<SetVariable>")
                    (\_ -> setEnv aWorld vpA W (Just theValue) cState)
                    theExistence

crsDeleteVariable :: Input -> VPosition -> Env
crsDeleteVariable anInput@(aWorld, _, cState) vp = newCState
 where
  theExistence = getMValue anInput vp
  theDLogMsg =
    T.append "[Fail]<crsSetVariable> A variable does not exists at" (showt vp)
  newCState = maybe (dLogAndErr anInput theDLogMsg (VP AtNull VINull) "")
                    (\_ -> setEnv aWorld vp W Nothing cState)
                    theExistence

crsModifyValue1 :: Input -> VPosition -> CERESOperator -> Env
crsModifyValue1 anInput@(aWorld, _, cState) vp cOp = newCState
 where
  theValue              = getValue anInput vp
  newValue              = modify1 cOp theValue
  (ErrValue theDLogMsg) = newValue
  newCState             = if getValueType newValue == VTErr
    then dLogAndErr anInput theDLogMsg vp theDLogMsg
    else setEnv aWorld vp W (Just newValue) cState

crsModifyValue2 :: Input -> VPosition -> VPosition -> CERESOperator -> Env
crsModifyValue2 anInput@(aWorld, _, cState) vpA vpB cOp = newCState
 where
  theVA                 = getValue anInput vpA
  theVB                 = getValue anInput vpB
  newValue              = modify2 cOp theVA theVB
  (ErrValue theDLogMsg) = newValue
  newCState             = if getValueType newValue == VTErr
    then dLogAndErr anInput theDLogMsg vpA theDLogMsg
    else setEnv aWorld vpA W (Just newValue) cState

crsModifyValue3
  :: Input -> VPosition -> VPosition -> CERESOperator -> VPosition -> Env
crsModifyValue3 anInput@(aWorld, _, cState) vpA vpB cOp vpC = newCState
 where
  theVA                 = getValue anInput vpA
  theVB                 = getValue anInput vpB
  theVC                 = getValue anInput vpC
  newValue              = modify3 cOp theVA theVB theVC
  (ErrValue theDLogMsg) = newValue
  newCState             = if getValueType newValue == VTErr
    then dLogAndErr anInput theDLogMsg vpA theDLogMsg
    else setEnv aWorld vpA W (Just newValue) cState

crsCopyValue :: Input -> VPosition -> VPosition -> Env
crsCopyValue anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  theValue     = getValue anInput vpB
  theExistence = getMValue anInput vpA
  theDLogMsg =
    T.append "[Fail]<crsCopyValue> A variable does not exists at" (showt vpA)
  newCState = maybe (dLogAndErr anInput theDLogMsg vpA "[Fail]<CopyValue>")
                    (\_ -> setEnv aWorld vpA W (Just theValue) cState)
                    theExistence

crsConvertValue :: Input -> VPosition -> ValueType -> Env
crsConvertValue anInput@(aWorld, _, cState) vp vt = newCState
 where
  theValue     = getValue anInput vp
  theExistence = getMValue anInput vp
  theDLogMsg =
    T.append "[Fail]<crsConvertValue> A variable does not exists at" (showt vp)
  newValue  = convertValue theValue vt
  newCState = maybe (dLogAndErr anInput theDLogMsg vp "[Fail]<ConvertValue>")
                    (\_ -> setEnv aWorld vp W (Just newValue) cState)
                    theExistence

crsConvertValueBy :: Input -> VPosition -> VPosition -> Env
crsConvertValueBy anInput vpA vpB = crsConvertValue anInput vpA vt
  where vt = getValueType $ getValue anInput vpB

crsConvertValueWith :: Input -> VPosition -> VPosition -> Env
crsConvertValueWith anInput@(aWorld, aSI, cState) vpA vpB = undefined

crsReplaceText :: Input -> VPosition -> Env
crsReplaceText anInput vp = crsReplaceTextTo anInput vp vp

crsReplaceTextTo :: Input -> VPosition -> VPosition -> Env
crsReplaceTextTo anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  targetStr     = getStr $ getValue anInput vpA
  eReplacingStr = findVariables targetStr
  dLogMsg       = T.append "Fail to read VariablePosition: "
  newCState     = either
    (\msg -> dLogAndErr anInput (dLogMsg msg) vpB (dLogMsg msg))
    (\str ->
      setEnv aWorld vpB W (Just . StrValue . replaceStr anInput $ str) cState
    )
    eReplacingStr

crsGetVPosition :: Input -> VPosition -> VPosition -> Env
crsGetVPosition anInput@(aWorld, _, cState) vpA vpB = newCState
 where
  targetStr     = getValue anInput vpA
  lazyTargetStr = TL.fromStrict . getStr $ targetStr
  rTargetPtr    = parseVariablePosition lazyTargetStr
  mTargetPtr    = getResult rTargetPtr
  aMsg          = fromJust . getMessage $ rTargetPtr
  theDLogMsg    = T.append "Fail to read VariablePosition: " (showt targetStr)
  newCState     = maybe (dLogAndErr anInput theDLogMsg vpB aMsg)
                        (\p -> setEnv aWorld vpB W (Just . PtrValue $ p) cState)
                        mTargetPtr

crsSetVPosition :: Input -> VPosition -> VPosition -> Env
crsSetVPosition anInput@(aWorld, _, cState) vpA vpB =
  setEnv aWorld vpB W (Just . PtrValue $ vpA) cState

crsRandom :: Input -> VPosition -> ValueType -> Env
crsRandom (aWorld, _, cState@Env {..}) vp vType =
  setEnv aWorld vp W (Just rValue) . setRG nextRG $ cState
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
crsRandomWith anInput@(aWorld@World {..}, aSI@SI {..}, cState) vpA vType vpC vpD vpE
  = notYetImpl "crsRandomWith"

crsRandomWithBy
  :: Input
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> VPosition
  -> Env
crsRandomWithBy anInput@(aWorld@World {..}, aSI@SI {..}, cState) vpA vpB vpC vpD vpE
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
crsElapsedTime anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vpA vpB
  = undefined
 where
  -- NOTE: 1. Estimate next instruction's executing internal time
  -- NOTE: 2. If elapsedTime + executingTime > worldTSSize, then store executingTime in localVariables and end interpreting up
  -- NOTE: 3. Else, do interpret, and modify (+executingTime) elapsedTime
  -- NOTE: Calculate executingTime not runCEREScript but here, because length of executingTime is depends on an instruction
  (executingTime, rg1) = bmwrInt worldTSSize rg
  elapsedTime =
    maybe 0 getInt . IM.lookup elapsedInternalTimeIdx $ lVars lCache
  newElapsedTime = elapsedTime + executingTime
  doSkip         = newElapsedTime > worldTSSize
  elapsedInternalTime =
    maybe False getBool $ IM.lookup resumeCodeIdx $ lVars lCache

crsSPControl :: Input -> VPosition -> Env
crsSPControl anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vp =
  undefined

crsSIControl :: Input -> VPosition -> VPosition -> Env
crsSIControl anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vpA vpB
  = undefined

-- NOTE: crsSIInit does not initiate SI by itself, but `runSpoolInstance` do it
crsSIInit :: Input -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsSIInit anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vpA vpB vpC vpD
  = undefined
  where spoolID = undefined

crsSIEnd :: Input -> VPosition -> Env
crsSIEnd anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vp =
  undefined

crsNoop :: Input -> Env
crsNoop (_, _, cState) = cState

crsLog :: Input -> VPosition -> VPosition -> Env
crsLog anInput@(aWorld, _, cState@Env {..}) vpA@(VP ~AtTricky ~(VIN logTarget)) vpB
  = setTCache (vNMapInsert logTarget logV tCache) cState
  where logV@(StrValue logMsg) = getValue anInput vpB


crsParseScript :: Input -> VPosition -> VPosition -> Env
crsParseScript anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) vpA vpB
  = undefined

crsToInterpreter0 :: Input -> CHeader -> Env
crsToInterpreter0 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader
  = undefined

crsToInterpreter1 :: Input -> CHeader -> VPosition -> Env
crsToInterpreter1 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA
  = undefined

crsToInterpreter2 :: Input -> CHeader -> VPosition -> VPosition -> Env
crsToInterpreter2 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB
  = undefined

crsToInterpreter3
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> Env
crsToInterpreter3 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC
  = undefined

crsToInterpreter4
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsToInterpreter4 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD
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
crsToInterpreter5 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE
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
crsToInterpreter6 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF
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
crsToInterpreter7 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF vpG
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
crsToInterpreter8 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined


crsExtend0 :: Input -> CHeader -> Env
crsExtend0 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader =
  undefined

crsExtend1 :: Input -> CHeader -> VPosition -> Env
crsExtend1 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA
  = undefined

crsExtend2 :: Input -> CHeader -> VPosition -> VPosition -> Env
crsExtend2 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB
  = undefined

crsExtend3 :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> Env
crsExtend3 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC
  = undefined

crsExtend4
  :: Input -> CHeader -> VPosition -> VPosition -> VPosition -> VPosition -> Env
crsExtend4 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD
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
crsExtend5 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE
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
crsExtend6 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF
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
crsExtend7 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF vpG
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
crsExtend8 anInput@(aWorld@World {..}, aSI@SI {..}, cState@Env {..}) iHeader vpA vpB vpC vpD vpE vpF vpG vpH
  = undefined
