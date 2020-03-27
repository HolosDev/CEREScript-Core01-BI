module CERES.BI.Interpret.Cache where

import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.List                      ( nub
                                                , partition
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.VariablePosition

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Spool

import           Debug
import           Util


-- TODO: Not using foldr, but Refactoring to use getValues* instead of getValue
-- TODO: But using getValues* is hard to use
cacheMaker :: SpoolTree -> World -> WorldCache
cacheMaker SpoolTree {..} World {..} = S.foldr cacheMakerSub blankCache vpSet
 where
  blankCache = (IM.empty, IM.empty, IM.empty)
  cacheMakerSub vp aCache = case vp of
    (AtWrld time idx) ->
      let mValue                   = getHValueFromWS worldState time idx
          (hCache, dCache, vCache) = aCache
          newHCache                = setHCache time idx R mValue hCache
      in  (newHCache, dCache, vCache)
    (AtTime time idx) ->
      let mValue = getHValueFromWS worldState (worldTime + time) idx
          (hCache, dCache, vCache) = aCache
          newHCache = setHCache worldTime idx R mValue hCache
      in  (newHCache, dCache, vCache)
    (AtDict idx) ->
      let mValue                   = getDValueFromWS worldState idx
          (hCache, dCache, vCache) = aCache
          newDCache                = setRWMVMap idx R mValue dCache
      in  (hCache, newDCache, vCache)
    (AtNDic nKey) -> notYetImpl "cacheMaker :=: AtNDict"
    (AtVars idx) ->
      let mValue                   = getVValueFromWS worldState idx
          (hCache, dCache, vCache) = aCache
          newVCache                = setRWMVMap idx R mValue vCache
      in  (hCache, dCache, newVCache)
    (AtLocl _) -> aCache
    (AtCach _) -> aCache
    (AtHere _) -> aCache
    AtNull     -> aCache
    _          -> error $ "[ERROR]<cacheMaker> Not compatible for " ++ show vp

-- TODO: Add NDict after adding NDict field
setEnv
  :: World -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnv World {..} = setEnvBy worldTime

-- TODO: Add NDict after adding NDict field
setEnvBy
  :: Time -> VPosition -> (Maybe Value -> RWMV) -> Maybe Value -> Env -> Env
setEnvBy _ vp@(AtWrld time idx) mode mValue ((hCache, dCache, vCache), localVars, localCache, rg)
  = ((newHCache, dCache, vCache), localVars, localCache, rg)
  where newHCache = setHCache time idx mode mValue hCache
setEnvBy worldTime vp@(AtTime time idx) mode mValue ((hCache, dCache, vCache), localVars, localCache, rg)
  = ((newHCache, dCache, vCache), localVars, localCache, rg)
  where newHCache = setHCache (worldTime + time) idx mode mValue hCache
setEnvBy _ (AtDict idx) mode mValue ((hCache, dCache, vCache), localVars, localCache, rg)
  = ((hCache, newDCache, vCache), localVars, localCache, rg)
  where newDCache = setRWMVMap idx mode mValue dCache
setEnvBy _ (AtNDic nKey) mode mValue ((hCache, dCache, vCache), localVars, localCache, rg)
  = notYetImpl "setEnvBy:=:AtNDict"
setEnvBy _ (AtVars idx) mode mValue ((hCache, dCache, vCache), localVars, localCache, rg)
  = ((hCache, dCache, newVCache), localVars, localCache, rg)
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
  -> HistoricCache
  -> HistoricCache
setHCache time idx mode mValue hCache = newHCache
 where
  rwmvMap    = fromMaybe IM.empty (IM.lookup time hCache)
  newRWMVMap = setRWMVMap idx mode mValue rwmvMap
  newHCache  = IM.insert time newRWMVMap hCache

setRWMVMap :: Idx -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVMap -> RWMVMap
setRWMVMap idx mode mValue = IM.insert idx (mode mValue)

setVMap :: Idx -> Maybe Value -> ValueMap -> ValueMap
setVMap idx mValue = IM.update (const mValue) idx


getEnv :: World -> VPosition -> Env -> Value
getEnv World {..} = getEnvBy worldTime

getEnvBy :: Time -> VPosition -> Env -> Value
getEnvBy _ vp@(AtWrld time idx) ((hCache, _, _), _, _, _) =
  getHCache time idx hCache
getEnvBy worldTime vp@(AtTime time idx) ((hCache, _, _), _, _, _) =
  getHCache (worldTime + time) idx hCache
getEnvBy _ (AtDict idx) ((_, dCache, _), _, _, _) = getRWMVMap idx dCache
-- TODO: Need to implement
getEnvBy _ (AtNDic nKey) ((_, dCache, _), _, _, _) =
  notYetImpl "getEnvBy:=:AtNDict"
getEnvBy _ (AtVars idx) ((_, _, vCache), _, _, _) = getRWMVMap idx vCache
getEnvBy _ (AtLocl idx) (_, localVars, _, _) = getVMap idx localVars
getEnvBy _ (AtCach idx) (_, _, localCache, _) = getVMap idx localCache
getEnvBy _ (AtHere v) (_, _, _, _) = v
getEnvBy _ AtNull (_, _, _, _) =
  error "[ERROR]<getEnvBy :=: AtNull> Can't access AtNull"

getHCache :: Time -> Idx -> HistoricCache -> Value
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

getVMap :: Idx -> ValueMap -> Value
getVMap id vMap = fromMaybe (ErrValue "[ERROR]<getVMap> No such Value") found
 where
  found :: Maybe Value
  found = IM.lookup id vMap


-- TODO: This style is for foldr, we may change this better
-- TODO: Change this for when many WorldCache is given as List or etc.
cacheCommitter :: WorldCache -> WorldState -> WorldState
cacheCommitter (hCache, dCache, vCache) aWorldState@WorldState {..} =
  newWorldState
 where
  newWorldState =
    updateWorldState aWorldState newWorldHistory newWorldDict newWorldVars
  newWorldHistory = updateWorldHistoryFromCache worldHistory hCache
  newWorldDict    = updateValuesToValueMap worldDict (unwrapFromRWMV dCache)
  newWorldVars    = updateValuesToValueMap worldDict (unwrapFromRWMV vCache)

unwrapFromRWMV :: RWMVMap -> [(Idx, Maybe Value)]
unwrapFromRWMV = map (second runRW) . filter (notR . snd) . IM.toList

-- NOTE: HistoricCache could have values in a time-slot which HistoricTable may not have
-- NOTE: Anyway, every values should alive
-- TODO: Optimize unique key generator
-- TODO: Optimize with/without updateValuesToVT
updateWorldHistoryFromCache :: HistoricTable -> HistoricCache -> HistoricTable
updateWorldHistoryFromCache historicTable hCache = IM.map newRow uniqueTimes
 where
  uniqueTimes =
    IM.fromList
      .  map (\x -> (x, x))
      .  nub
      $  IM.keys historicTable
      ++ IM.keys hCache
  newRow time = EpochRow time newValues
   where
    baseRow     = maybe IM.empty values . IM.lookup time $ historicTable
    targetCache = fromMaybe IM.empty . IM.lookup time $ hCache
    unwrapped   = unwrapFromRWMV targetCache
    newValues   = updateValuesToValueMap baseRow unwrapped
