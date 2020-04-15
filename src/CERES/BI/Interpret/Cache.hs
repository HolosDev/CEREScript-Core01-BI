module CERES.BI.Interpret.Cache where


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
  blankCache = (IM.empty, IM.empty, IM.empty, Trie.empty, IM.empty, Trie.empty)
  cacheMakerSub vp aCache = case vp of
    (VP AtWorld ~(VIIT idx time)) ->
      let mValue    = getHValueFromWS worldState time idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newHCache = setHCache time idx R mValue hCache
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
      let mValue     = getNValueFromWS worldState nKey
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newNDCache = setRWMVNMap nKey R mValue nDCache
      in  (hCache, nHCache, dCache, newNDCache, vCache, nVCache)
    (VP AtVars ~(VII idx)) ->
      let mValue    = getVValueFromWS worldState idx
          (hCache, nHCache, dCache, nDCache, vCache, nVCache) = aCache
          newVCache = setRWMVMap idx R mValue vCache
      in  (hCache, nHCache, dCache, nDCache, newVCache, nVCache)
    (VP AtLVars ~(VII idx)) -> aCache
    (VP AtLTemp ~(VII idx)) -> aCache
    (VP AtHere  _         ) -> aCache
    (VP AtNull  _         ) -> aCache
    _ -> error $ "[ERROR]<cacheMaker> Not compatible for " ++ show vp



-- TODO: This style is for foldr, we may change this better
-- TODO: Change this for when many WorldCache is given as List or etc.
cacheCommitter :: WorldCache -> WorldState -> WorldState
cacheCommitter (hCache, nHCache, dCache, nDCache, vCache, nVCache) aWorldState@WorldState {..}
  = newWorldState
 where
  newWorldState = undefined
    $ updateWorldState aWorldState newWorldHistory newWorldDict newWorldVars
  newWorldHistory = updateWorldHistoryFromCache worldHistory hCache
  newWorldDict    = updateValuesToValueMap worldDict (unwrapFromRWMV dCache)
  newWorldVars    = updateValuesToValueMap worldDict (unwrapFromRWMV vCache)

-- NOTE: HistoricalCache could have values in a time-slot which HistoricalTable may not have
-- NOTE: Anyway, every values should alive
-- TODO: Optimize unique key generator
-- TODO: Optimize with/without updateValuesToVT
updateWorldHistoryFromCache
  :: HistoricalTable -> HistoricalCache -> HistoricalTable
updateWorldHistoryFromCache historicalTable hCache = IM.map newRow uniqueTimes
 where
  uniqueTimes =
    IM.fromList
      .  map (\x -> (x, x))
      .  nub
      $  IM.keys historicalTable
      ++ IM.keys hCache
  newRow time = EpochRow time newValues
   where
    baseRow     = maybe IM.empty values . IM.lookup time $ historicalTable
    targetCache = fromMaybe IM.empty . IM.lookup time $ hCache
    unwrapped   = unwrapFromRWMV targetCache
    newValues   = updateValuesToValueMap baseRow unwrapped
