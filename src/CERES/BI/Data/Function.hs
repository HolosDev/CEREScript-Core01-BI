module CERES.BI.Data.Function where


import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data
import           CERES.BI.Data.Constants

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( groupBy
                                                , sortBy
                                                )
import           Data.Maybe


updateWorldState
  :: WorldState -> HistoricTable -> Dictionary -> Variables -> WorldState
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

getHValueFromVT :: HistoricTable -> Time -> Idx -> Maybe Value
getHValueFromVT worldHistory time idx =
  IM.lookup time worldHistory >>= (IM.lookup idx . values)

getHValuesFromWS
  :: WorldState -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromWS WorldState {..} = getHValuesFromVT worldHistory

getHValuesFromVT
  :: HistoricTable -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromVT worldHistory indices = map
  (getHValuesFromVTSub worldHistory)
  grouped
 where
  getTime = fst
  sorted  = sortBy (\x y -> compare (getTime x) (getTime y)) indices
  grouped = groupBy (\x y -> getTime x == getTime y) sorted

getHValuesFromVTSub
  :: HistoricTable -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromVTSub aHistoricTable indices = getHValuesFromVTSubSub aValues
                                                                    indices
 where
  theTime = fst . head $ indices
  aValues = maybe IM.empty values (IM.lookup theTime aHistoricTable)

getHValuesFromVTSubSub
  :: Values -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromVTSubSub aValues =
  map (\(t, idx) -> ((t, idx), IM.lookup idx aValues))


updateValueToWS :: WorldState -> Time -> Idx -> Maybe Value -> WorldState
updateValueToWS worldState time idx aMValue = worldState
  { worldHistory = updateValueToVT (worldHistory worldState) time idx aMValue
  }

updateValueToVT :: HistoricTable -> Time -> Idx -> Maybe Value -> HistoricTable
updateValueToVT worldHistory time idx aMValue = newHistoricTable
 where
  baseValues       = maybe IM.empty values . IM.lookup time $ worldHistory
  newEpochRow      = EpochRow time (IM.update (const aMValue) idx baseValues)
  newHistoricTable = IM.insert time newEpochRow worldHistory

updateValuesToWS :: WorldState -> [((Time, Idx), Maybe Value)] -> HistoricTable
updateValuesToWS WorldState {..} = updateValuesToVT worldHistory

updateValuesToVT
  :: HistoricTable -> [((Time, Idx), Maybe Value)] -> HistoricTable
updateValuesToVT worldHistory ivList = newHistoricTable
 where
  getTime          = fst . fst
  sorted           = sortBy (\x y -> compare (getTime x) (getTime y)) ivList
  grouped          = groupBy (\x y -> getTime x == getTime y) sorted
  newHistoricTable = foldr updateValuesToVTSub worldHistory grouped

-- TODO: Should be parallel when update existing element
-- NOTE: But, inserting new element should be serialized
updateValuesToVTSub
  :: [((Time, Idx), Maybe Value)] -> HistoricTable -> HistoricTable
updateValuesToVTSub ivList aHistoricTable = newHistoricTable
 where
  theTime = fst . fst . head $ ivList
  aEpochRow =
    fromMaybe (EpochRow theTime IM.empty) (IM.lookup theTime aHistoricTable)
  newEpochRow =
    aEpochRow { values = updateValuesToVTSubSub (values aEpochRow) ivList }
  newHistoricTable = IM.insert theTime newEpochRow aHistoricTable

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
