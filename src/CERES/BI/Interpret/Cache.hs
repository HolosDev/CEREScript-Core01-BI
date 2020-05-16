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


import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Cache.Function
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.SpoolTree

import           CERES.BI.Type

import           Debug


-- TODO: Not using foldr, but Refactoring to use getValues* instead of getValue
-- TODO: But using getValues* is hard to use
worldCacheMaker :: SpoolTree -> World -> WorldCache
worldCacheMaker SpoolTree {..} World {..} = S.foldr cacheMakerSub
                                                    blankWorldCache
                                                    vpSet
 where
  cacheMakerSub vp aWCache = case vp of
    (VP AtWorld (VII idx)) ->
      let mValue    = getHValueFromWS worldState 0 idx
          newHCache = setValueInHCache 0 idx R mValue (hCache aWCache)
      in  aWCache { hCache = newHCache }
    (VP AtWorld ~(VIIT idx time)) ->
      let mValue    = getHValueFromWS worldState time idx
          newHCache = setValueInHCache time idx R mValue (hCache aWCache)
      in  aWCache { hCache = newHCache }
    (VP AtTime (VII idx)) ->
      let mValue    = getHValueFromWS worldState worldTime idx
          newHCache = setValueInHCache worldTime idx R mValue (hCache aWCache)
      in  aWCache { hCache = newHCache }
    (VP AtTime ~(VIIT idx time)) ->
      let mValue    = getHValueFromWS worldState (worldTime + time) idx
          newHCache = setValueInHCache worldTime idx R mValue (hCache aWCache)
      in  aWCache { hCache = newHCache }
    (VP AtNWorld (VIN nKey)) ->
      let mValue     = getNHValueFromWS worldState 0 nKey
          newNHCache = setValueInNHCache 0 nKey R mValue (nHCache aWCache)
      in  aWCache { nHCache = newNHCache }
    (VP AtNWorld ~(VINT nKey time)) ->
      let mValue     = getNHValueFromWS worldState time nKey
          newNHCache = setValueInNHCache time nKey R mValue (nHCache aWCache)
      in  aWCache { nHCache = newNHCache }
    (VP AtNTime (VIN nKey)) ->
      let mValue = getNHValueFromWS worldState worldTime nKey
          newNHCache =
              setValueInNHCache worldTime nKey R mValue (nHCache aWCache)
      in  aWCache { nHCache = newNHCache }
    (VP AtNTime ~(VINT nKey time)) ->
      let mValue = getNHValueFromWS worldState (worldTime + time) nKey
          newNHCache =
              setValueInNHCache worldTime nKey R mValue (nHCache aWCache)
      in  aWCache { nHCache = newNHCache }
    (VP AtVars ~(VII idx)) ->
      let mValue    = getVValueFromWS worldState idx
          newVCache = setRWMVMap idx R mValue (vCache aWCache)
      in  aWCache { vCache = newVCache }
    (VP AtNVars ~(VIN nKey)) ->
      let mValue     = getNVValueFromWS worldState nKey
          newNVCache = setRWMVNMap nKey R mValue (nVCache aWCache)
      in  aWCache { nVCache = newNVCache }
    (VP AtDict ~(VII idx)) ->
      let mValue    = getDValueFromWS worldState idx
          newDCache = setRWMVMap idx R mValue (dCache aWCache)
      in  aWCache { dCache = newDCache }
    (VP AtNDict ~(VIN nKey)) ->
      let mValue     = getNDValueFromWS worldState nKey
          newNDCache = setRWMVNMap nKey R mValue (nDCache aWCache)
      in  aWCache { nDCache = newNDCache }
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
    (VP AtHere _) -> error $ "[ERROR]<cacheMaker :=: AtHere> Can't be reached"
    (VP AtNull _) -> error $ "[ERROR]<cacheMaker :=: AtNull> Can't be reached"
    _ -> error $ "[ERROR]<cacheMaker> Not compatible for " ++ show vp



-- TODO: This style is for foldr, we may change this better
-- TODO: Change this for when many WorldCache is given as List or etc.
worldCacheCommitter :: WorldCache -> WorldState -> WorldState
worldCacheCommitter WorldCache {..} aWorldState@WorldState {..} = newWorldState
 where
  newWorldState =
    newWorldHistory
      `par`  newWorldNHistory
      `par`  newWorldVars
      `par`  newWorldNVars
      `par`  newWorldDict
      `par`  newWorldNDict
      `pseq` updateWorldState aWorldState
                              newWorldHistory
                              newWorldNHistory
                              newWorldVars
                              newWorldNVars
                              newWorldDict
                              newWorldNDict
  newWorldHistory = updateWorldHistoryFromCache worldHistory hCache
  newWorldNHistory = updateWorldNHistoryFromCache worldNHistory nHCache
  newWorldVars = updateValuesToValueMap worldVars (unwrapFromRWMV vCache)
  newWorldNVars = updateValuesToValueNMap worldNVars (unwrapFromRWMVN nVCache)
  newWorldDict = updateValuesToValueMap worldDict (unwrapFromRWMV dCache)
  newWorldNDict = updateValuesToValueNMap worldNDict (unwrapFromRWMVN nDCache)

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
