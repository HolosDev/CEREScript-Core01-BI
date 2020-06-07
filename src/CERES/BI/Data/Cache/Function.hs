module CERES.BI.Data.Cache.Function where


import           Data.Bifunctor
import qualified Data.IntMap                   as IM
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe
import qualified Data.Text.Lazy                as TL
import qualified Data.Trie.Text                as Trie


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Function
import           CERES.BI.Data.Environment
import           CERES.BI.Type
import           CERES.BI.Util

import           Debug
import           Util


-- TODO: Add NDict after adding NDict field
setEnv
  :: World -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnv World {..} = setEnvBy worldTime

setWCache :: WorldCache -> Env -> Env
setWCache aWCache aEnv = aEnv { wCache = aWCache }
setLCache :: LocalCache -> Env -> Env
setLCache aLCache aEnv = aEnv { lCache = aLCache }
setTCache :: TrickCache -> Env -> Env
setTCache aTCache aEnv = aEnv { tCache = aTCache }
setRG :: RG -> Env -> Env
setRG aRG aEnv = aEnv { rg = aRG }

setHCache :: HistoricalCache -> Env -> Env
setHCache aHCache aEnv@Env {..} =
  let newWCache = wCache { hCache = aHCache } in aEnv { wCache = newWCache }

setNHCache :: NHistoricalCache -> Env -> Env
setNHCache aNHCache aEnv@Env {..} =
  let newWCache = wCache { nHCache = aNHCache } in aEnv { wCache = newWCache }

setVCache :: VariableCache -> Env -> Env
setVCache aVCache aEnv@Env {..} =
  let newWCache = wCache { vCache = aVCache } in aEnv { wCache = newWCache }

setNVCache :: NVariableCache -> Env -> Env
setNVCache aNVCache aEnv@Env {..} =
  let newWCache = wCache { nVCache = aNVCache } in aEnv { wCache = newWCache }

setDCache :: DictionaryCache -> Env -> Env
setDCache aDCache aEnv@Env {..} =
  let newWCache = wCache { dCache = aDCache } in aEnv { wCache = newWCache }

setNDCache :: NDictionaryCache -> Env -> Env
setNDCache aNDCache aEnv@Env {..} =
  let newWCache = wCache { nDCache = aNDCache } in aEnv { wCache = newWCache }

setLVars :: LocalVariables -> Env -> Env
setLVars aLVars aEnv@Env {..} =
  let newLCache = lCache { lVars = aLVars } in aEnv { lCache = newLCache }

setLNVars :: LocalNVariables -> Env -> Env
setLNVars aLNVars aEnv@Env {..} =
  let newLCache = lCache { lNVars = aLNVars } in aEnv { lCache = newLCache }

setLTemp :: LocalTemp -> Env -> Env
setLTemp aLTemp aEnv@Env {..} =
  let newLCache = lCache { lTemp = aLTemp } in aEnv { lCache = newLCache }

setLNTemp :: LocalNTemp -> Env -> Env
setLNTemp aLNTemp aEnv@Env {..} =
  let newLCache = lCache { lNTemp = aLNTemp } in aEnv { lCache = newLCache }


-- TODO: Add NDict after adding NDict field
setEnvBy
  :: Time -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnvBy _ vp@(VP AtWorld (VII idx)) mode mValue aEnv@Env {..} = setHCache
  newHCache
  aEnv
  where newHCache = setValueInHCache 0 idx mode mValue (hCache wCache)
setEnvBy _ vp@(VP AtWorld ~(VIIT idx time)) mode mValue aEnv@Env {..} =
  setHCache newHCache aEnv
  where newHCache = setValueInHCache time idx mode mValue (hCache wCache)
setEnvBy worldTime vp@(VP AtTime (VII idx)) mode mValue aEnv@Env {..} =
  setHCache newHCache aEnv
  where newHCache = setValueInHCache worldTime idx mode mValue (hCache wCache)
setEnvBy worldTime vp@(VP AtTime ~(VIIT idx time)) mode mValue aEnv@Env {..} =
  setHCache newHCache aEnv
 where
  newHCache =
    setValueInHCache (worldTime + time) idx mode mValue (hCache wCache)
setEnvBy _ vp@(VP AtNWorld (VIN nKey)) mode mValue aEnv@Env {..} = setNHCache
  newNHCache
  aEnv
  where newNHCache = setValueInNHCache 0 nKey mode mValue (nHCache wCache)
setEnvBy _ vp@(VP AtNWorld ~(VINT nKey time)) mode mValue aEnv@Env {..} =
  setNHCache newNHCache aEnv
  where newNHCache = setValueInNHCache time nKey mode mValue (nHCache wCache)
setEnvBy worldTime vp@(VP AtNTime (VIN nKey)) mode mValue aEnv@Env {..} =
  setNHCache newNHCache aEnv
 where
  newNHCache = setValueInNHCache worldTime nKey mode mValue (nHCache wCache)
setEnvBy worldTime vp@(VP AtNTime ~(VINT nKey time)) mode mValue aEnv@Env {..}
  = setNHCache newNHCache aEnv
 where
  newNHCache =
    setValueInNHCache (worldTime + time) nKey mode mValue (nHCache wCache)
setEnvBy _ (VP AtVars ~(VII idx)) mode mValue aEnv@Env {..} = setVCache
  newVCache
  aEnv
  where newVCache = setRWMVMap idx mode mValue (vCache wCache)
setEnvBy _ (VP AtNVars ~(VIN nKey)) mode mValue aEnv@Env {..} = setNVCache
  newNVCache
  aEnv
  where newNVCache = setRWMVNMap nKey mode mValue (nVCache wCache)
setEnvBy _ (VP AtDict ~(VII idx)) mode mValue aEnv@Env {..} = setDCache
  newDCache
  aEnv
  where newDCache = setRWMVMap idx mode mValue (dCache wCache)
setEnvBy _ (VP AtNDict ~(VIN nKey)) mode mValue aEnv@Env {..} = setNDCache
  newNDCache
  aEnv
  where newNDCache = setRWMVNMap nKey mode mValue (nDCache wCache)
setEnvBy _ (VP AtLVars ~(VII idx)) mode mValue aEnv@Env {..} = setLVars
  newLVars
  aEnv
  where newLVars = vMapUpdate idx mValue (lVars lCache)
setEnvBy _ (VP AtLNVars ~(VIN nKey)) mode mValue aEnv@Env {..} = setLNVars
  newLNVars
  aEnv
  where newLNVars = vNMapUpdate nKey mValue (lNVars lCache)
setEnvBy _ (VP AtLTemp ~(VII idx)) mode mValue aEnv@Env {..} = setLTemp
  newLTemp
  aEnv
  where newLTemp = vMapUpdate idx mValue (lTemp lCache)
setEnvBy _ (VP AtLNTemp ~(VIN nKey)) mode mValue aEnv@Env {..} = setLNTemp
  newLNTemp
  aEnv
  where newLNTemp = vNMapUpdate nKey mValue (lNTemp lCache)
setEnvBy _ (VP AtTricky ~(VIN nKey)) mode mValue aEnv@Env {..} = aEnv
  { tCache = newTrickCache
  }
  where newTrickCache = vNMapUpdate nKey mValue tCache
setEnvBy _ _ _ _ cState = cState


setValueInHCache
  :: Time
  -> Idx
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> HistoricalCache
  -> HistoricalCache
setValueInHCache time idx mode mValue hCache = newHCache
 where
  rwmvMap    = fromMaybe IM.empty (IM.lookup time hCache)
  newRWMVMap = setRWMVMap idx mode mValue rwmvMap
  newHCache  = IM.insert time newRWMVMap hCache

setValueInNHCache
  :: Time
  -> NKey
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> NHistoricalCache
  -> NHistoricalCache
setValueInNHCache time nKey mode mValue nHCache = newNHCache
 where
  rwmvNMap    = fromMaybe blankVNM (IM.lookup time nHCache)
  newRWMVNMap = setRWMVNMap nKey mode mValue rwmvNMap
  newNHCache  = IM.insert time newRWMVNMap nHCache

setRWMVMap :: Idx -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVMap -> RWMVMap
setRWMVMap idx mode mValue = IM.insert idx (mode mValue)

setRWMVNMap
  :: NKey -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVNMap -> RWMVNMap
setRWMVNMap nKey mode mValue = Trie.insert nKey (mode mValue)


getValue :: Input -> VPosition -> Value
getValue anInput vp@(VP AtWorld (VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtWorld[VII]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtWorld ~(VIIT idx time)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtWorld[VIIT]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNWorld (VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNWorld[VIN]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNWorld ~(VINT nKey time)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNWorld[VINT]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtTime (VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtTime[VII]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtTime ~(VIIT idx time)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtTime[VIIT] > No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNTime (VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNTime[VIN]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNTime ~(VINT nKey time)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNTime[VINT]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtVars ~(VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtVars[VII]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNVars ~(VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNVars[VIN]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtDict ~(VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtDict[VII]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtNDict ~(VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtNDict[VIN]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtLVars ~(VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtLVars[VII]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtLNVars ~(VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtLNVars[VIN]> No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtLTemp ~(VII idx)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtLTemp[VII] > No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtLNTemp ~(VIN nKey)) = fromMaybe
  (error $ "[ERROR]<getValue :=: AtLNTemp[VIN] > No such value at " <> show vp)
  (getMValue anInput vp)
getValue anInput vp@(VP AtHere   ~(VIV v)) = v
-- TODO: Need to implement
getValue anInput vp@(VP AtTricky _       ) = fromMaybe
  (error $ "[ERROR]<getValue :=: > No such value at " <> show vp)
  (Just $ error "[ERROR]<getValue :=: AtTricky> Not yet implemented")
getValue anInput (VP AtPtr ~(VIV ~(PtrValue pVP))) = getValue anInput pVP
getValue _ (VP AtReg _) =
  error "[ERROR]<getValue :=: AtReg> Not yet implemented"
getValue _ (VP AtNull _) =
  error "[ERROR]<getValue :=: AtNull> Can't access AtNull"
getValue _ vp = error $ "[ERROR]<getValue> Can't be reached with " <> show vp


-- FIXME: Fix when refers non-cached value
getMValue :: Input -> VPosition -> Maybe Value
getMValue (World {..}, _, Env {..}) vp@(VP AtWorld (VII idx)) =
  recover (getHCache 0 idx (hCache wCache)) (getHValueFromWS worldState 0 idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtWorld ~(VIIT idx time)) = recover
  (getHCache time idx (hCache wCache))
  (getHValueFromWS worldState time idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtNWorld (VIN nKey)) = recover
  (getNHCache 0 nKey (nHCache wCache))
  (getNHValueFromWS worldState 0 nKey)
getMValue (World {..}, _, Env {..}) vp@(VP AtNWorld ~(VINT nKey time)) =
  recover (getNHCache time nKey (nHCache wCache))
          (getNHValueFromWS worldState time nKey)
getMValue (World {..}, _, Env {..}) vp@(VP AtTime (VII idx)) = recover
  (getHCache worldTime idx (hCache wCache))
  (getHValueFromWS worldState worldTime idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtTime ~(VIIT idx time)) = recover
  (getHCache (worldTime + time) idx (hCache wCache))
  (getHValueFromWS worldState (worldTime + time) idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtNTime (VIN nKey)) = recover
  (getNHCache worldTime nKey (nHCache wCache))
  (getNHValueFromWS worldState worldTime nKey)
getMValue (World {..}, _, Env {..}) vp@(VP AtNTime ~(VINT nKey time)) = recover
  (getNHCache (worldTime + time) nKey (nHCache wCache))
  (getNHValueFromWS worldState (worldTime + time) nKey)
getMValue (World {..}, _, Env {..}) vp@(VP AtVars ~(VII idx)) =
  recover (getRWMVMap idx (vCache wCache)) (getVValueFromWS worldState idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtNVars ~(VIN nKey)) =
  recover (getRWMVNMap nKey (nVCache wCache)) (getNVValueFromWS worldState nKey)
getMValue (World {..}, _, Env {..}) vp@(VP AtDict ~(VII idx)) =
  recover (getRWMVMap idx (dCache wCache)) (getDValueFromWS worldState idx)
getMValue (World {..}, _, Env {..}) vp@(VP AtNDict ~(VIN nKey)) =
  recover (getRWMVNMap nKey (nDCache wCache)) (getNDValueFromWS worldState nKey)
getMValue (_, _, Env {..}) vp@(VP AtLVars ~(VII idx)) =
  vMapLookup idx (lVars lCache)
getMValue (_, _, Env {..}) vp@(VP AtLNVars ~(VIN nKey)) =
  vNMapLookup nKey (lNVars lCache)
getMValue (_, _, Env {..}) vp@(VP AtLTemp ~(VII idx)) =
  vMapLookup idx (lTemp lCache)
getMValue (_, _, Env {..}) vp@(VP AtLNTemp ~(VIN nKey)) =
  vNMapLookup nKey (lNTemp lCache)
getMValue _ vp@(VP AtHere ~(VIV v)) = Just v
-- TODO: Need to implement
getMValue (aWorld, aSI, _) vp@(VP AtTricky _) =
  Just $ error "[ERROR]<getMValue :=: AtTricky> Not yet implemented"
getMValue anInput (VP AtPtr ~(VIV ~(PtrValue pVP))) = getMValue anInput pVP
getMValue _ (VP AtReg _) =
  error "[ERROR]<getMValue :=: AtReg> Not yet implemented"
getMValue _ (VP AtNull _) =
  error "[ERROR]<getMValue :=: AtNull> Can't access AtNull"
getMValue _ vp = error $ "[ERROR]<getMValue> Can't be reached with " <> show vp

getHCache :: Time -> Idx -> HistoricalCache -> Maybe Value
getHCache time idx hCache = IM.lookup time hCache >>= IM.lookup idx >>= runRW

getNHCache :: Time -> NKey -> NHistoricalCache -> Maybe Value
getNHCache time nKey nHCache =
  IM.lookup time nHCache >>= Trie.lookup nKey >>= runRW

getRWMVMap :: Idx -> RWMVMap -> Maybe Value
getRWMVMap idx rwmvMap = IM.lookup idx rwmvMap >>= runRW

getRWMVNMap :: NKey -> RWMVNMap -> Maybe Value
getRWMVNMap nKey rwmvnMap = Trie.lookup nKey rwmvnMap >>= runRW

unwrapFromRWMV :: RWMVMap -> [(Idx, Maybe Value)]
unwrapFromRWMV = map (second runRW) . filter (notR . snd) . IM.toList

unwrapFromRWMVN :: RWMVNMap -> [(NKey, Maybe Value)]
unwrapFromRWMVN =
  map (bimap TL.toStrict runRW) . filter (notR . snd) . Trie.toList

unwrapFromRWMVNH :: RWMVNHMap -> [(NKey, Maybe Value)]
unwrapFromRWMVNH = map (second runRW) . filter (notR . snd) . HM.toList
