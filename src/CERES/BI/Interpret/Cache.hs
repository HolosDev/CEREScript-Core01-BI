module CERES.BI.Interpret.Cache where

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
import           CERES.BI.Data.Cache.Function
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
