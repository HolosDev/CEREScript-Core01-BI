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

import           Data.CERES.Script
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.VariablePosition

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
setEnvBy _ vp@(AtWrld time idx) mode mValue ((hCache, dCache, nCache, vCache), localVars, localCache, rg)
  = ((newHCache, dCache, nCache, vCache), localVars, localCache, rg)
  where newHCache = setHCache time idx mode mValue hCache
setEnvBy worldTime vp@(AtTime time idx) mode mValue ((hCache, dCache, nCache, vCache), localVars, localCache, rg)
  = ((newHCache, dCache, nCache, vCache), localVars, localCache, rg)
  where newHCache = setHCache (worldTime + time) idx mode mValue hCache
setEnvBy _ (AtDict idx) mode mValue ((hCache, dCache, nCache, vCache), localVars, localCache, rg)
  = ((hCache, newDCache, nCache, vCache), localVars, localCache, rg)
  where newDCache = setRWMVMap idx mode mValue dCache
setEnvBy _ (AtNDic nKey) mode mValue ((hCache, dCache, nCache, vCache), localVars, localCache, rg)
  = ((hCache, dCache, newNCache, vCache), localVars, localCache, rg)
  where newNCache = setRWMVNMap nKey mode mValue nCache
setEnvBy _ (AtVars idx) mode mValue ((hCache, dCache, nCache, vCache), localVars, localCache, rg)
  = ((hCache, dCache, nCache, newVCache), localVars, localCache, rg)
  where newVCache = setRWMVMap idx mode mValue vCache
setEnvBy _ (AtLocl idx) mode mValue (wCache, localVars, localCache, rg) =
  (wCache, newLocalVars, localCache, rg)
  where newLocalVars = setVMap idx mValue localVars
setEnvBy _ (AtCach idx) mode mValue (wCache, localVars, localCache, rg) =
  (wCache, localVars, newLocalCache, rg)
  where newLocalCache = setVMap idx mValue localCache
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

setRWMVNMap :: NKey -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVNMap -> RWMVNMap
setRWMVNMap nKey mode mValue = Trie.insert (TL.toStrict nKey) (mode mValue)

setVMap :: Idx -> Maybe Value -> ValueMap -> ValueMap
setVMap idx mValue = IM.update (const mValue) idx


getEnv :: World -> VPosition -> Env -> Value
getEnv World {..} = getEnvBy worldTime

getEnvBy :: Time -> VPosition -> Env -> Value
getEnvBy _ vp@(AtWrld time idx) ((hCache, _, _, _), _, _, _) =
  getHCache time idx hCache
getEnvBy worldTime vp@(AtTime time idx) ((hCache, _, _, _), _, _, _) =
  getHCache (worldTime + time) idx hCache
getEnvBy _ (AtDict idx) ((_, dCache, _, _), _, _, _) = getRWMVMap idx dCache
-- TODO: Need to implement
getEnvBy _ (AtNDic nKey) ((_, _, nCache, _), _, _, _) = getRWMVNMap nKey nCache
getEnvBy _ (AtVars idx) ((_, _, _, vCache), _, _, _) = getRWMVMap idx vCache
getEnvBy _ (AtLocl idx) (_, localVars, _, _) = getVMap idx localVars
getEnvBy _ (AtCach idx) (_, _, localCache, _) = getVMap idx localCache
getEnvBy _ (AtHere v) (_, _, _, _) = v
getEnvBy _ AtNull (_, _, _, _) =
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
  found = Trie.lookup (TL.toStrict nKey) rwmvnMap >>= runRW

getVMap :: Idx -> ValueMap -> Value
getVMap id vMap = fromMaybe (ErrValue "[ERROR]<getVMap> No such Value") found
 where
  found :: Maybe Value
  found = IM.lookup id vMap

unwrapFromRWMV :: RWMVMap -> [(Idx, Maybe Value)]
unwrapFromRWMV = map (second runRW) . filter (notR . snd) . IM.toList
