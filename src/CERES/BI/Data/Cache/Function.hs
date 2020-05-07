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

-- TODO: Add NDict after adding NDict field
setEnvBy
  :: Time -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnvBy _ vp@(VP AtWorld (VII idx)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache 0 idx mode mValue hCache
setEnvBy _ vp@(VP AtWorld ~(VIIT idx time)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache time idx mode mValue hCache
setEnvBy worldTime vp@(VP AtTime (VII idx)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache worldTime idx mode mValue hCache
setEnvBy worldTime vp@(VP AtTime ~(VIIT idx time)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache (worldTime + time) idx mode mValue hCache
setEnvBy _ vp@(VP AtNWorld (VIN nKey)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache 0 nKey mode mValue nHCache
setEnvBy _ vp@(VP AtNWorld ~(VINT nKey time)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache time nKey mode mValue nHCache
setEnvBy worldTime vp@(VP AtNTime (VIN nKey)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache worldTime nKey mode mValue nHCache
setEnvBy worldTime vp@(VP AtNTime ~(VINT nKey time)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, vCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache (worldTime + time) nKey mode mValue nHCache
setEnvBy _ (VP AtVars ~(VII idx)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, newVCache, nVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newVCache = setRWMVMap idx mode mValue vCache
setEnvBy _ (VP AtNVars ~(VIN nKey)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, vCache, newNVCache, dCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNVCache = setRWMVNMap nKey mode mValue nVCache
setEnvBy _ (VP AtDict ~(VII idx)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, vCache, nVCache, newDCache, nDCache)
    , localCache
    , trickCache
    , rg
    )
  where newDCache = setRWMVMap idx mode mValue dCache
setEnvBy _ (VP AtNDict ~(VIN nKey)) mode mValue ((hCache, nHCache, vCache, nVCache, dCache, nDCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, vCache, nVCache, dCache, newNDCache)
    , localCache
    , trickCache
    , rg
    )
  where newNDCache = setRWMVNMap nKey mode mValue nDCache
setEnvBy _ (VP AtLVars ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (newLVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  where newLVCache = vMapUpdate idx mValue lVCache
setEnvBy _ (VP AtLNVars ~(VIN nKey)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, newLNVCache, lTCache, lNTCache), trickCache, rg)
  where newLNVCache = vNMapUpdate nKey mValue lNVCache
setEnvBy _ (VP AtLTemp ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, lNVCache, newLTCache, lNTCache), trickCache, rg)
  where newLTCache = vMapUpdate idx mValue lTCache
setEnvBy _ (VP AtLNTemp ~(VIN nKey)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, lNVCache, lTCache, newLNTCache), trickCache, rg)
  where newLNTCache = vNMapUpdate nKey mValue lNTCache
setEnvBy _ (VP AtTricky ~(VIN nKey)) mode mValue (wCache, lCache, trickCache, rg)
  = (wCache, lCache, newTrickCache, rg)
  where newTrickCache = vNHMapUpdate nKey mValue trickCache
setEnvBy _ _ _ _ cState = cState


setHCache
  :: Time
  -> Idx
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> HistoricalCache
  -> HistoricalCache
setHCache time idx mode mValue hCache = newHCache
 where
  rwmvMap    = fromMaybe IM.empty (IM.lookup time hCache)
  newRWMVMap = setRWMVMap idx mode mValue rwmvMap
  newHCache  = IM.insert time newRWMVMap hCache

setNHCache
  :: Time
  -> NKey
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> NHistoricalCache
  -> NHistoricalCache
setNHCache time nKey mode mValue nHCache = newNHCache
 where
  rwmvNMap    = fromMaybe blankVNM (IM.lookup time nHCache)
  newRWMVNMap = setRWMVNMap nKey mode mValue rwmvNMap
  newNHCache  = IM.insert time newRWMVNMap nHCache

setRWMVMap :: Idx -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVMap -> RWMVMap
setRWMVMap idx mode mValue = IM.insert idx (mode mValue)

setRWMVNMap
  :: NKey -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVNMap -> RWMVNMap
setRWMVNMap nKey mode mValue = Trie.insert nKey (mode mValue)


-- FIXME: Fix when refers non-cached value
getValue :: Input -> VPosition -> Value
getValue (World {..}, _, ((hCache, _, _, _, _, _), _, _, _)) vp@(VP AtWorld (VII idx))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtWorld[VII]> No such value at " ++ show vp)
    (recover (getHCache 0 idx hCache) (getHValueFromWS worldState 0 idx))
getValue (World {..}, _, ((hCache, _, _, _, _, _), _, _, _)) vp@(VP AtWorld ~(VIIT idx time))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtWorld[VIIT]> No such value at " ++ show vp)
    (recover (getHCache time idx hCache) (getHValueFromWS worldState time idx))
getValue (World {..}, _, ((_, nHCache, _, _, _, _), _, _, _)) vp@(VP AtNWorld (VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNWorld[VIN]> No such value at " ++ show vp)
    (recover (getNHCache 0 nKey nHCache) (getNHValueFromWS worldState 0 nKey))
getValue (World {..}, _, ((_, nHCache, _, _, _, _), _, _, _)) vp@(VP AtNWorld ~(VINT nKey time))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNWorld[VINT]> No such value at " ++ show vp
    )
    (recover (getNHCache time nKey nHCache)
             (getNHValueFromWS worldState time nKey)
    )
getValue (World {..}, _, ((hCache, _, _, _, _, _), _, _, _)) vp@(VP AtTime (VII idx))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtTime[VII]> No such value at " ++ show vp)
    (recover (getHCache worldTime idx hCache)
             (getHValueFromWS worldState worldTime idx)
    )
getValue (World {..}, _, ((hCache, _, _, _, _, _), _, _, _)) vp@(VP AtTime ~(VIIT idx time))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtTime[VIIT] > No such value at " ++ show vp)
    (recover (getHCache (worldTime + time) idx hCache)
             (getHValueFromWS worldState (worldTime + time) idx)
    )
getValue (World {..}, _, ((_, nHCache, _, _, _, _), _, _, _)) vp@(VP AtNTime (VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNTime[VIN]> No such value at " ++ show vp)
    (recover (getNHCache worldTime nKey nHCache)
             (getNHValueFromWS worldState worldTime nKey)
    )
getValue (World {..}, _, ((_, nHCache, _, _, _, _), _, _, _)) vp@(VP AtNTime ~(VINT nKey time))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNTime[VINT]> No such value at " ++ show vp)
    (recover (getNHCache (worldTime + time) nKey nHCache)
             (getNHValueFromWS worldState (worldTime + time) nKey)
    )
getValue (World {..}, _, ((_, _, vCache, _, _, _), _, _, _)) vp@(VP AtVars ~(VII idx))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtVars[VII]> No such value at " ++ show vp)
    (recover (getRWMVMap idx vCache) (getVValueFromWS worldState idx))
getValue (World {..}, _, ((_, _, _, nVCache, _, _), _, _, _)) vp@(VP AtNVars ~(VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNVars[VIN]> No such value at " ++ show vp)
    (recover (getRWMVNMap nKey nVCache) (getNVValueFromWS worldState nKey))
getValue (World {..}, _, ((_, _, _, _, dCache, _), _, _, _)) vp@(VP AtDict ~(VII idx))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtDict[VII]> No such value at " ++ show vp)
    (recover (getRWMVMap idx dCache) (getDValueFromWS worldState idx))
getValue (World {..}, _, ((_, _, _, _, _, nDCache), _, _, _)) vp@(VP AtNDict ~(VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtNDict[VIN]> No such value at " ++ show vp)
    (recover (getRWMVNMap nKey nDCache) (getNDValueFromWS worldState nKey))
getValue (_, _, (_, (localVars, _, _, _), _, _)) vp@(VP AtLVars ~(VII idx)) =
  fromMaybe
    (error $ "[ERROR]<getValue :=: AtLVars[VII]> No such value at " ++ show vp)
    (vMapLookup idx localVars)
getValue (_, _, (_, (_, localNVars, _, _), _, _)) vp@(VP AtLNVars ~(VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtLNVars[VIN]> No such value at " ++ show vp)
    (vNMapLookup nKey localNVars)
getValue (_, _, (_, (_, _, localCache, _), _, _)) vp@(VP AtLTemp ~(VII idx)) =
  fromMaybe
    (error $ "[ERROR]<getValue :=: AtLTemp[VII] > No such value at " ++ show vp)
    (vMapLookup idx localCache)
getValue (_, _, (_, (_, _, _, localNCache), _, _)) vp@(VP AtLNTemp ~(VIN nKey))
  = fromMaybe
    (error $ "[ERROR]<getValue :=: AtLNTemp[VIN] > No such value at " ++ show vp
    )
    (vNMapLookup nKey localNCache)
getValue (_     , _  , (_, _, _, _)) vp@(VP AtHere   ~(VIV v)) = v
-- TODO: Need to implement
getValue (aWorld, aSI, (_, _, _, _)) vp@(VP AtTricky _       ) = fromMaybe
  (error $ "[ERROR]<getValue :=: > No such value at " ++ show vp)
  (Just $ error "[ERROR]<getValue :=: AtTricky> Not yet implemented")
getValue anInput (VP AtPtr ~(VIV ~(PtrValue pVP))) = getValue anInput pVP
getValue _ (VP AtReg _) =
  error "[ERROR]<getValue :=: AtReg> Not yet implemented"
getValue _ (VP AtNull _) =
  error "[ERROR]<getValue :=: AtNull> Can't access AtNull"
getValue _ vp = error $ "[ERROR]<getValue> Can't be reached with " ++ show vp

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
