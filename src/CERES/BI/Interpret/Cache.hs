module CERES.BI.Interpret.Cache where


import           Control.Parallel

import qualified Data.IntMap                   as IM
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.List                      ( nub
                                                , partition
                                                )
import qualified Data.Trie.Text                as Trie
import qualified Data.Set                      as S


import           CERES.Operate

import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Type

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
  blankCache = (IM.empty, IM.empty, blankVM, blankVNM, blankVM, blankVNM)
  cacheMakerSub vp aCache = case vp of
    (VP AtWorld (VII idx)) ->
      let mValue    = getHValueFromWS worldState 0 idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newHCache = setHCache 0 idx R mValue hCache
      in  (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    (VP AtWorld ~(VIIT idx time)) ->
      let mValue    = getHValueFromWS worldState time idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newHCache = setHCache time idx R mValue hCache
      in  (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    (VP AtTime (VII idx)) ->
      let mValue    = getHValueFromWS worldState worldTime idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newHCache = setHCache worldTime idx R mValue hCache
      in  (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    (VP AtTime ~(VIIT idx time)) ->
      let mValue    = getHValueFromWS worldState (worldTime + time) idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newHCache = setHCache worldTime idx R mValue hCache
      in  (newHCache, nHCache, dCache, nDCache, vCache, nVCache)
    (VP AtDict ~(VII idx)) ->
      let mValue    = getDValueFromWS worldState idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newDCache = setRWMVMap idx R mValue dCache
      in  (hCache, nHCache, newDCache, nDCache, vCache, nVCache)
    (VP AtNDict ~(VIN nKey)) ->
      let mValue     = getNDValueFromWS worldState nKey
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newNDCache = setRWMVNMap nKey R mValue nDCache
      in  (hCache, nHCache, dCache, newNDCache, vCache, nVCache)
    (VP AtVars ~(VII idx)) ->
      let mValue    = getVValueFromWS worldState idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newVCache = setRWMVMap idx R mValue vCache
      in  (hCache, nHCache, dCache, nDCache, newVCache, nVCache)
    (VP AtNVars ~(VIN nKey)) ->
      let mValue     = getNVValueFromWS worldState nKey
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newNVCache = setRWMVNMap nKey R mValue nVCache
      in  (hCache, nHCache, dCache, nDCache, vCache, newNVCache)
    (VP AtPtr _) -> error $ "[ERROR]<cacheMaker :=: AtPtr> Not yet implemented"
    (VP AtTricky _) ->
      error $ "[ERROR]<cacheMaker :=: AtTricky> Not yet implemented"
    (VP AtLVars _) ->
      error $ "[ERROR]<cacheMaker :=: AtLVars> Can't be reached"
    (VP AtLNVars _) ->
      error $ "[ERROR]<cacheMaker :=: AtLNVars> Can't be reached"
    (VP AtLTemp _) ->
      error $ "[ERROR]<cacheMaker :=: AtLTemp> Can't be reached"
    (VP AtLNTemp _) ->
      error $ "[ERROR]<cacheMaker :=: AtLNTemp> Can't be reached"
    (VP AtHere _) -> aCache
    (VP AtNull _) -> aCache
    _ -> error $ "[ERROR]<cacheMaker> Not compatible for " ++ show vp



-- TODO: This style is for foldr, we may change this better
-- TODO: Change this for when many WorldCache is given as List or etc.
cacheCommitter :: WorldCache -> WorldState -> WorldState
cacheCommitter (hCache, nHCache, dCache, nDCache, vCache, nVCache) aWorldState@WorldState {..}
  = newWorldState
 where
  newWorldState =
    newWorldHistory
      `par`  newWorldNHistory
      `par`  newWorldDict
      `par`  newWorldNDict
      `par`  newWorldVars
      `par`  newWorldNVars
      `pseq` updateWorldState aWorldState
                              newWorldHistory
                              newWorldNHistory
                              newWorldDict
                              newWorldNDict
                              newWorldVars
                              newWorldNVars
  newWorldHistory = updateWorldHistoryFromCache worldHistory hCache
  newWorldNHistory = updateWorldNHistoryFromCache worldNHistory nHCache
  newWorldDict = updateValuesToValueMap worldDict (unwrapFromRWMV dCache)
  newWorldNDict = updateValuesToValueNMap worldNDict (unwrapFromRWMVN nDCache)
  newWorldVars = updateValuesToValueMap worldVars (unwrapFromRWMV vCache)
  newWorldNVars = updateValuesToValueNMap worldNVars (unwrapFromRWMVN nVCache)

-- NOTE: HistoricalCache could have values in a time-slot which HistoricalTable may not have
-- NOTE: Anyway, every values should alive
-- TODO: Optimize unique key generator
-- TODO: Optimize with/without updateValuesToVT
updateWorldHistoryFromCache
  :: HistoricalTable -> HistoricalCache -> HistoricalTable
updateWorldHistoryFromCache aHistoricalTable hCache = IM.map newRow uniqueTimes
 where
  uniqueTimes =
    IM.fromList
      .  map (\x -> (x, x))
      .  nub
      $  IM.keys aHistoricalTable
      ++ IM.keys hCache
  newRow time = EpochRow time newValues
   where
    baseRow     = maybe blankVM values . IM.lookup time $ aHistoricalTable
    targetCache = fromMaybe blankVM . IM.lookup time $ hCache
    unwrapped   = unwrapFromRWMV targetCache
    newValues   = updateValuesToValueMap baseRow unwrapped

updateWorldNHistoryFromCache
  :: NHistoricalTable -> NHistoricalCache -> NHistoricalTable
updateWorldNHistoryFromCache aNHistoricalTable nHCache = IM.map newRow
                                                                uniqueTimes
 where
  uniqueTimes =
    IM.fromList
      .  map (\x -> (x, x))
      .  nub
      $  IM.keys aNHistoricalTable
      ++ IM.keys nHCache
  newRow time = NEpochRow time newNValues
   where
    baseRow     = maybe blankVNM nValues . IM.lookup time $ aNHistoricalTable
    targetCache = fromMaybe blankVNM . IM.lookup time $ nHCache
    unwrapped   = unwrapFromRWMVN targetCache
    newNValues  = updateValuesToValueNMap baseRow unwrapped
