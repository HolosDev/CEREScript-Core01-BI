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
  cacheMakerSub VP {..} aCache = case variablePlace of
    AtWorld time ->
      let
        mValue                   = getHValueFromWS worldState time variableID
        (hCache, dCache, vCache) = aCache
        newHCache =
          setHCache worldTime variablePlace variableID R mValue hCache
      in
        (newHCache, dCache, vCache)
    AtTime time ->
      let
        mValue = getHValueFromWS worldState (worldTime + time) variableID
        (hCache, dCache, vCache) = aCache
        newHCache =
          setHCache worldTime variablePlace variableID R mValue hCache
      in
        (newHCache, dCache, vCache)
    AtDict ->
      let mValue                   = getDValueFromWS worldState variableID
          (hCache, dCache, vCache) = aCache
          newDCache                = setRWMVMap variableID R mValue dCache
      in  (hCache, newDCache, vCache)
    AtVar ->
      let mValue                   = getVValueFromWS worldState variableID
          (hCache, dCache, vCache) = aCache
          newVCache                = setRWMVMap variableID R mValue vCache
      in  (hCache, dCache, newVCache)
    _ ->
      error $ "[ERROR]<cacheMaker> Not compatible for " ++ show variablePlace

setHCache
  :: Time
  -> VariablePlace
  -> ID
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> HistoricCache
  -> HistoricCache
setHCache _         (AtWorld time) = setHCacheSub time
setHCache worldTime (AtTime  time) = setHCacheSub (worldTime + time)

setHCacheSub
  :: Time
  -> ID
  -> (Maybe Value -> RWMV)
  -> Maybe Value
  -> HistoricCache
  -> HistoricCache
setHCacheSub time idx mode mValue hCache = newHCache
 where
  vMap :: IntMap RWMV
  vMap = fromMaybe IM.empty (IM.lookup time hCache)
  newVMap :: IntMap RWMV
  newVMap   = IM.insert idx (mode mValue) vMap
  newHCache = IM.insert time newVMap hCache

setRWMVMap :: ID -> (Maybe Value -> RWMV) -> Maybe Value -> RWMVMap -> RWMVMap
setRWMVMap idx mode mValue = IM.insert idx (mode mValue)

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

unwrapFromRWMV :: RWMVMap -> [(ID, Maybe Value)]
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
