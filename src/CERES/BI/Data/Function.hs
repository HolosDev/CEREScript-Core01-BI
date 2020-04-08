module CERES.BI.Data.Function where


import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data
import           CERES.BI.Data.Constants


import           Data.Function
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( groupBy
                                                , sortBy
                                                )
import           Data.Maybe
import qualified Data.Text.Lazy                as TL
import qualified Data.Trie.Text                as Trie


updateWorldState
  :: WorldState -> HistoricalTable -> Dictionary -> Variables -> WorldState
updateWorldState ws newWorldHistory newWorldDict newWorldVars = ws
  { worldHistory = newWorldHistory
  , worldDict    = newWorldDict
  , worldVars    = newWorldVars
  }

updateWorld :: World -> WorldState -> SpoolInstanceTable -> Time -> World
updateWorld aWorld newWorldState newSITable newWorldTime = aWorld
  { worldState   = newWorldState
  , worldSITable = newSITable
  , worldTime    = newWorldTime
  }

-- TODO: Move sub functions in a nest
getHValueFromWS :: WorldState -> Time -> Idx -> Maybe Value
getHValueFromWS worldState = getHValueFromVT (worldHistory worldState)

getHValueFromVT :: HistoricalTable -> Time -> Idx -> Maybe Value
getHValueFromVT worldHistory time idx =
  IM.lookup time worldHistory >>= (IM.lookup idx . values)

getHValuesFromWS
  :: WorldState -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromWS WorldState {..} = getHValuesFromVT worldHistory

getHValuesFromVT
  :: HistoricalTable -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromVT worldHistory indices = map
  (getHValuesFromVTSub worldHistory)
  grouped
 where
  getTime = fst
  sorted  = sortBy (compare `on` getTime) indices
  grouped = groupBy ((==) `on` getTime) sorted

getHValuesFromVTSub
  :: HistoricalTable -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromVTSub aHistoricalTable indices = getHValuesFromVTSubSub aValues
                                                                      indices
 where
  theTime = fst . head $ indices
  aValues = maybe IM.empty values (IM.lookup theTime aHistoricalTable)

getHValuesFromVTSubSub
  :: Values -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromVTSubSub aValues =
  map (\(t, idx) -> ((t, idx), IM.lookup idx aValues))


updateValueToWS :: WorldState -> Time -> Idx -> Maybe Value -> WorldState
updateValueToWS worldState time idx aMValue = worldState
  { worldHistory = updateValueToVT (worldHistory worldState) time idx aMValue
  }

updateValueToVT
  :: HistoricalTable -> Time -> Idx -> Maybe Value -> HistoricalTable
updateValueToVT worldHistory time idx aMValue = newHistoricalTable
 where
  baseValues         = maybe IM.empty values . IM.lookup time $ worldHistory
  newEpochRow        = EpochRow time (IM.update (const aMValue) idx baseValues)
  newHistoricalTable = IM.insert time newEpochRow worldHistory

updateValuesToWS
  :: WorldState -> [((Time, Idx), Maybe Value)] -> HistoricalTable
updateValuesToWS WorldState {..} = updateValuesToVT worldHistory

updateValuesToVT
  :: HistoricalTable -> [((Time, Idx), Maybe Value)] -> HistoricalTable
updateValuesToVT worldHistory ivList = newHistoricalTable
 where
  getTime            = fst . fst
  sorted             = sortBy (compare `on` getTime) ivList
  grouped            = groupBy ((==) `on` getTime) sorted
  newHistoricalTable = foldr updateValuesToVTSub worldHistory grouped

-- TODO: Should be parallel when update existing element
-- NOTE: But, inserting new element should be serialized
updateValuesToVTSub
  :: [((Time, Idx), Maybe Value)] -> HistoricalTable -> HistoricalTable
updateValuesToVTSub ivList aHistoricalTable = newHistoricalTable
 where
  theTime = fst . fst . head $ ivList
  aEpochRow =
    fromMaybe (EpochRow theTime IM.empty) (IM.lookup theTime aHistoricalTable)
  newEpochRow =
    aEpochRow { values = updateValuesToVTSubSub (values aEpochRow) ivList }
  newHistoricalTable = IM.insert theTime newEpochRow aHistoricalTable

updateValuesToVTSubSub :: Values -> [((Time, Idx), Maybe Value)] -> Values
updateValuesToVTSubSub =
  foldr (\((_, idx), aMValue) v -> IM.update (const aMValue) idx v)


getDValueFromWS :: WorldState -> Idx -> Maybe Value
getDValueFromWS WorldState {..} = getValueFromValueMap worldDict

getVValueFromWS :: WorldState -> Idx -> Maybe Value
getVValueFromWS WorldState {..} = getValueFromValueMap worldVars

getValueFromValueMap :: ValueMap -> Idx -> Maybe Value
getValueFromValueMap valueMap idx = IM.lookup idx valueMap

getDValuesFromWS :: WorldState -> [Idx] -> [(Idx, Maybe Value)]
getDValuesFromWS WorldState {..} = getValuesFromValueMap worldDict

getVValuesFromWS :: WorldState -> [Idx] -> [(Idx, Maybe Value)]
getVValuesFromWS WorldState {..} = getValuesFromValueMap worldVars

getValuesFromValueMap :: ValueMap -> [Idx] -> [(Idx, Maybe Value)]
getValuesFromValueMap valueMap = map (\idx -> (idx, IM.lookup idx valueMap))


updateDValueToWS :: WorldState -> Idx -> Maybe Value -> WorldState
updateDValueToWS ws@WorldState {..} idx aMValue =
  ws { worldDict = updateValueToValueMap worldDict idx aMValue }

updateVValueToWS :: WorldState -> Idx -> Maybe Value -> WorldState
updateVValueToWS ws@WorldState {..} idx aMValue =
  ws { worldDict = updateValueToValueMap worldVars idx aMValue }

updateValueToValueMap :: ValueMap -> Idx -> Maybe Value -> ValueMap
updateValueToValueMap valueMap idx aMValue =
  IM.update (const aMValue) idx valueMap

updateValuesToDict :: WorldState -> [(Idx, Maybe Value)] -> WorldState
updateValuesToDict ws@WorldState {..} ivList =
  ws { worldDict = updateValuesToValueMap worldDict ivList }

updateValuesToVar :: WorldState -> [(Idx, Maybe Value)] -> WorldState
updateValuesToVar ws@WorldState {..} ivList =
  ws { worldDict = updateValuesToValueMap worldVars ivList }

updateValuesToValueMap :: ValueMap -> [(Idx, Maybe Value)] -> ValueMap
updateValuesToValueMap = foldr (\(i, v) -> IM.update (const v) i)




getNValueFromWS :: WorldState -> NKey -> Maybe Value
getNValueFromWS WorldState {..} = getValueFromValueNMap worldNDict

getValueFromValueNMap :: ValueNMap -> NKey -> Maybe Value
getValueFromValueNMap valueNMap nKey = Trie.lookup nKey valueNMap

getNValuesFromWS :: WorldState -> [NKey] -> [(NKey, Maybe Value)]
getNValuesFromWS WorldState {..} = getValuesFromValueNMap worldNDict

getValuesFromValueNMap :: ValueNMap -> [NKey] -> [(NKey, Maybe Value)]
getValuesFromValueNMap valueNMap =
  map (\nKey -> (nKey, Trie.lookup nKey valueNMap))


updateNValueToWS :: WorldState -> NKey -> Maybe Value -> WorldState
updateNValueToWS ws@WorldState {..} nKey aMValue =
  ws { worldNDict = updateValueToValueNMap worldNDict nKey aMValue }

updateValueToValueNMap :: ValueNMap -> NKey -> Maybe Value -> ValueNMap
updateValueToValueNMap valueNMap nKey aMValue = case aMValue of
  (Just aValue) -> Trie.adjust (const aValue) nKey valueNMap
  _             -> Trie.delete nKey valueNMap

updateValuesToNDic :: WorldState -> [(NKey, Maybe Value)] -> WorldState
updateValuesToNDic ws@WorldState {..} ivList =
  ws { worldNDict = updateValuesToValueNMap worldNDict ivList }

updateValuesToValueNMap :: ValueNMap -> [(NKey, Maybe Value)] -> ValueNMap
updateValuesToValueNMap = foldr (\(n, v) b -> updateValueToValueNMap b n v)
