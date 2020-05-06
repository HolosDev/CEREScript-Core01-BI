module CERES.BI.Data.Cache.Function where


import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe
import qualified Data.Text.Lazy                as TL
import qualified Data.Trie.Text                as Trie


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Function
import           CERES.BI.Data.Environment

import           Debug
import           Util


-- TODO: Add NDict after adding NDict field
setEnv
  :: World -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnv World {..} = setEnvBy worldTime

-- TODO: Add NDict after adding NDict field
setEnvBy
  :: Time -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnvBy _ vp@(VP AtWorld (VII idx)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache 0 idx mode mValue hCache
setEnvBy _ vp@(VP AtWorld ~(VIIT idx time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache time idx mode mValue hCache
setEnvBy worldTime vp@(VP AtTime (VII idx)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache worldTime idx mode mValue hCache
setEnvBy worldTime vp@(VP AtTime ~(VIIT idx time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache (worldTime + time) idx mode mValue hCache
setEnvBy _ vp@(VP AtNWorld (VIN nKey)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache 0 nKey mode mValue nHCache
setEnvBy _ vp@(VP AtNWorld ~(VINT nKey time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache time nKey mode mValue nHCache
setEnvBy worldTime vp@(VP AtNTime (VIN nKey)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache worldTime nKey mode mValue nHCache
setEnvBy worldTime vp@(VP AtNTime ~(VINT nKey time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, newNHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNHCache = setNHCache (worldTime + time) nKey mode mValue nHCache
setEnvBy _ (VP AtDict ~(VII idx)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, newDCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newDCache = setRWMVMap idx mode mValue dCache
setEnvBy _ (VP AtNDict ~(VIN nKey)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, dCache, newNDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNDCache = setRWMVNMap nKey mode mValue nDCache
setEnvBy _ (VP AtVars ~(VII idx)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, dCache, nDCache, newVCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newVCache = setRWMVMap idx mode mValue vCache
setEnvBy _ (VP AtNVars ~(VIN nKey)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (hCache, nHCache, dCache, nDCache, vCache, newNVCache)
    , localCache
    , trickCache
    , rg
    )
  where newNVCache = setRWMVNMap nKey mode mValue nVCache
setEnvBy _ (VP AtLVars ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (newLVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  where newLVCache = setVMap idx mValue lVCache
setEnvBy _ (VP AtLNVars ~(VIN nKey)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, newLNVCache, lTCache, lNTCache), trickCache, rg)
  where newLNVCache = setVNMap nKey mValue lNVCache
setEnvBy _ (VP AtLTemp ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, lNVCache, newLTCache, lNTCache), trickCache, rg)
  where newLTCache = setVMap idx mValue lTCache
setEnvBy _ (VP AtLNTemp ~(VIN nKey)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, lNVCache, lTCache, newLNTCache), trickCache, rg)
  where newLNTCache = setVNMap nKey mValue lNTCache
setEnvBy _ (VP AtTricky ~(VIN nKey)) mode mValue (wCache, lCache, trickCache, rg)
  = (wCache, lCache, newTrickCache, rg)
  where newTrickCache = setVNMap nKey mValue trickCache
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

setVMap :: Idx -> Maybe Value -> ValueMap -> ValueMap
setVMap idx mValue = IM.update (const mValue) idx

setVNMap :: NKey -> Maybe Value -> ValueNMap -> ValueNMap
setVNMap nKey mValue vnMap =
  maybe (Trie.delete nKey vnMap) (\v -> Trie.insert nKey v vnMap) mValue


-- FIXME: Fix when refers non-cached value
getEnv :: World -> VPosition -> Env -> Value
getEnv World {..} vp@(VP AtWorld (VII idx)) ((hCache, _, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtWorld[VII]> No such value at " ++ show vp)
    (recover (getHCache 0 idx hCache) (getHValueFromWS worldState 0 idx))
getEnv World {..} vp@(VP AtWorld ~(VIIT idx time)) ((hCache, _, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtWorld[VIIT]> No such value at " ++ show vp)
    (recover (getHCache time idx hCache) (getHValueFromWS worldState time idx))
getEnv World {..} vp@(VP AtNWorld (VIN nKey)) ((_, nHCache, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNWorld[VIN]> No such value at " ++ show vp)
    (recover (getNHCache 0 nKey nHCache) (getNHValueFromWS worldState 0 nKey))
getEnv World {..} vp@(VP AtNWorld ~(VINT nKey time)) ((_, nHCache, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNWorld[VINT]> No such value at " ++ show vp)
    (recover (getNHCache time nKey nHCache)
             (getNHValueFromWS worldState time nKey)
    )
getEnv World {..} vp@(VP AtTime (VII idx)) ((hCache, _, _, _, _, _), _, _, _) =
  fromMaybe
    (error $ "[ERROR]<getEnv :=: AtTime[VII]> No such value at " ++ show vp)
    (recover (getHCache worldTime idx hCache)
             (getHValueFromWS worldState worldTime idx)
    )
getEnv World {..} vp@(VP AtTime ~(VIIT idx time)) ((hCache, _, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtTime[VIIT] > No such value at " ++ show vp)
    (recover (getHCache (worldTime + time) idx hCache)
             (getHValueFromWS worldState (worldTime + time) idx)
    )
getEnv World {..} vp@(VP AtNTime (VIN nKey)) ((_, nHCache, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNTime[VIN]> No such value at " ++ show vp)
    (recover (getNHCache worldTime nKey nHCache)
             (getNHValueFromWS worldState worldTime nKey)
    )
getEnv World {..} vp@(VP AtNTime ~(VINT nKey time)) ((_, nHCache, _, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNTime[VINT]> No such value at " ++ show vp)
    (recover (getNHCache (worldTime + time) nKey nHCache)
             (getNHValueFromWS worldState (worldTime + time) nKey)
    )
getEnv World {..} vp@(VP AtDict ~(VII idx)) ((_, _, dCache, _, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtDict[VII]> No such value at " ++ show vp)
    (recover (getRWMVMap idx dCache) (getDValueFromWS worldState idx))
getEnv World {..} vp@(VP AtNDict ~(VIN nKey)) ((_, _, _, nDCache, _, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNDict[VIN]> No such value at " ++ show vp)
    (recover (getRWMVNMap nKey nDCache) (getNDValueFromWS worldState nKey))
getEnv World {..} vp@(VP AtVars ~(VII idx)) ((_, _, _, _, vCache, _), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtVars[VII]> No such value at " ++ show vp)
    (recover (getRWMVMap idx vCache) (getVValueFromWS worldState idx))
getEnv World {..} vp@(VP AtNVars ~(VIN nKey)) ((_, _, _, _, _, nVCache), _, _, _)
  = fromMaybe
    (error $ "[ERROR]<getEnv :=: AtNVars[VIN]> No such value at " ++ show vp)
    (recover (getRWMVNMap nKey nVCache) (getNVValueFromWS worldState nKey))
getEnv _ vp@(VP AtLVars ~(VII idx)) (_, (localVars, _, _, _), _, _) = fromMaybe
  (error $ "[ERROR]<getEnv :=: AtLVars[VII]> No such value at " ++ show vp)
  (getVMap idx localVars)
getEnv _ vp@(VP AtLNVars ~(VIN nKey)) (_, (_, localNVars, _, _), _, _) =
  fromMaybe
    (error $ "[ERROR]<getEnv :=: AtLNVars[VIN]> No such value at " ++ show vp)
    (getVNMap nKey localNVars)
getEnv _ vp@(VP AtLTemp ~(VII idx)) (_, (_, _, localCache, _), _, _) =
  fromMaybe
    (error $ "[ERROR]<getEnv :=: AtLTemp[VII] > No such value at " ++ show vp)
    (getVMap idx localCache)
getEnv _ vp@(VP AtLNTemp ~(VIN nKey)) (_, (_, _, _, localNCache), _, _) =
  fromMaybe
    (error $ "[ERROR]<getEnv :=: AtLNTemp[VIN] > No such value at " ++ show vp)
    (getVNMap nKey localNCache)
getEnv _      vp@(VP AtHere   ~(VIV v)) (_, _, _, _) = v
-- TODO: Need to implement
getEnv aWorld vp@(VP AtTricky _       ) (_, _, _, _) = fromMaybe
  (error $ "[ERROR]<getEnv :=: > No such value at " ++ show vp)
  (Just $ error "[ERROR]<getEnv :=: AtTricky> Not yet implemented")
getEnv aWorld (VP AtPtr _) (_, _, _, _) =
  error "[ERROR]<getEnv :=: AtPtr> Not yet implemented"
getEnv _ (VP AtReg _) (_, _, _, _) =
  error "[ERROR]<getEnv :=: AtReg> Not yet implemented"
getEnv _ (VP AtNull _) (_, _, _, _) =
  error "[ERROR]<getEnv :=: AtNull> Can't access AtNull"
getEnv _ vp _ = error $ "[ERROR]<getEnv> Can't be reached with " ++ show vp

getHCache :: Time -> Idx -> HistoricalCache -> Maybe Value
getHCache time idx hCache = IM.lookup time hCache >>= IM.lookup idx >>= runRW

getNHCache :: Time -> NKey -> NHistoricalCache -> Maybe Value
getNHCache time nKey nHCache =
  IM.lookup time nHCache >>= Trie.lookup nKey >>= runRW

getRWMVMap :: Idx -> RWMVMap -> Maybe Value
getRWMVMap idx rwmvMap = IM.lookup idx rwmvMap >>= runRW

getRWMVNMap :: NKey -> RWMVNMap -> Maybe Value
getRWMVNMap nKey rwmvnMap = Trie.lookup nKey rwmvnMap >>= runRW

getVMap :: Idx -> ValueMap -> Maybe Value
getVMap = IM.lookup

getVNMap :: NKey -> ValueNMap -> Maybe Value
getVNMap = Trie.lookup

unwrapFromRWMV :: RWMVMap -> [(Idx, Maybe Value)]
unwrapFromRWMV = map (second runRW) . filter (notR . snd) . IM.toList

unwrapFromRWMVN :: RWMVNMap -> [(NKey, Maybe Value)]
unwrapFromRWMVN =
  map (bimap TL.toStrict runRW) . filter (notR . snd) . Trie.toList
