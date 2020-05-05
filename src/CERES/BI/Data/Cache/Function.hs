module CERES.BI.Data.Cache.Function where


import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import           Data.Trie.Text                 ( Trie )
import qualified Data.Trie.Text                as Trie

import           TextShow


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data
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
setEnvBy _ vp@(VP AtWorld ~(VIIT idx time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache time idx mode mValue hCache
setEnvBy worldTime vp@(VP AtTime ~(VIIT idx time)) mode mValue ((hCache, nHCache, dCache, nDCache, vCache, nVCache), localCache, trickCache, rg)
  = ( (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    , localCache
    , trickCache
    , rg
    )
  where newHCache = setHCache (worldTime + time) idx mode mValue hCache
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
setEnvBy _ (VP AtLVars ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (newLVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  where newLVCache = setVMap idx mValue lVCache
setEnvBy _ (VP AtLTemp ~(VII idx)) mode mValue (wCache, (lVCache, lNVCache, lTCache, lNTCache), trickCache, rg)
  = (wCache, (lVCache, lNVCache, newLTCache, lNTCache), trickCache, rg)
  where newLTCache = setVMap idx mValue lTCache
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

setRWMVMap :: Idx -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVMap -> RWMVMap
setRWMVMap idx mode mValue = IM.insert idx (mode mValue)

setRWMVNMap
  :: NKey -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVNMap -> RWMVNMap
setRWMVNMap nKey mode mValue = Trie.insert nKey (mode mValue)

setVMap :: Idx -> Maybe Value -> ValueMap -> ValueMap
setVMap idx mValue = IM.update (const mValue) idx


getEnv :: World -> VPosition -> Env -> Value
getEnv World {..} = getEnvBy worldTime

getEnvBy :: Time -> VPosition -> Env -> Value
getEnvBy _ vp@(VP AtWorld ~(VIIT idx time)) ((hCache, _, _, _, _, _), _, _, _)
  = getHCache time idx hCache
getEnvBy worldTime vp@(VP AtTime ~(VIIT idx time)) ((hCache, _, _, _, _, _), _, _, _)
  = getHCache (worldTime + time) idx hCache
getEnvBy _ (VP AtDict ~(VII idx)) ((_, _, dCache, _, _, _), _, _, _) =
  getRWMVMap idx dCache
-- TODO: Need to implement
getEnvBy _ (VP AtNDict ~(VIN nKey)) ((_, _, _, nCache, _, _), _, _, _) =
  getRWMVNMap nKey nCache
getEnvBy _ (VP AtVars ~(VII idx)) ((_, _, _, _, vCache, _), _, _, _) =
  getRWMVMap idx vCache
getEnvBy _ (VP AtLVars ~(VII idx)) (_, (localVars, _, _, _), _, _) =
  getVMap idx localVars
getEnvBy _ (VP AtLTemp ~(VII idx)) (_, (_, _, localCache, _), _, _) =
  getVMap idx localCache
getEnvBy _ (VP AtHere ~(VIV v)) (_, _, _, _) = v
getEnvBy _ (VP AtNull _) (_, _, _, _) =
  error "[ERROR]<getEnvBy :=: AtNull> Can't access AtNull"

getHCache :: Time -> Idx -> HistoricalCache -> Value
getHCache time idx hCache = fromMaybe
  (ErrValue "[ERROR]<getHCacheSub> No such Value")
  found
 where
  found :: Maybe Value
  found = IM.lookup time hCache >>= IM.lookup idx >>= runRW

getRWMVMap :: Idx -> RWMVMap -> Value
getRWMVMap idx rwmvMap = fromMaybe
  (ErrValue "[ERROR]<getRWMVMap> No such Value")
  found
 where
  found :: Maybe Value
  found = IM.lookup idx rwmvMap >>= runRW

getRWMVNMap :: NKey -> RWMVNMap -> Value
getRWMVNMap nKey rwmvnMap = fromMaybe
  (ErrValue "[ERROR]<getRWMVNMap> No such Value")
  found
 where
  found :: Maybe Value
  found = Trie.lookup nKey rwmvnMap >>= runRW

getVMap :: Idx -> ValueMap -> Value
getVMap id vMap = fromMaybe (ErrValue "[ERROR]<getVMap> No such Value") found
 where
  found :: Maybe Value
  found = IM.lookup id vMap

unwrapFromRWMV :: RWMVMap -> [(Idx, Maybe Value)]
unwrapFromRWMV = map (second runRW) . filter (notR . snd) . IM.toList
