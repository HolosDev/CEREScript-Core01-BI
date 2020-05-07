module CERES.BI.Data.Function where


import           Data.Function
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( groupBy
                                                , sortBy
                                                )
import           Data.Maybe
import qualified Data.Trie.Text                as Trie


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Constants

import           CERES.BI.Type
import           CERES.BI.Util


updateWorldState
  :: WorldState
  -> HistoricalTable
  -> NHistoricalTable
  -> Variables
  -> NVariables
  -> Dictionary
  -> NDictionary
  -> WorldState
updateWorldState ws newHistoricalTable newNHistoricalTable newVars newNVars newDict newNDict
  = ws { worldHistory  = newHistoricalTable
       , worldNHistory = newNHistoricalTable
       , worldVars     = newVars
       , worldNVars    = newNVars
       , worldDict     = newDict
       , worldNDict    = newNDict
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

getNHValueFromWS :: WorldState -> Time -> NKey -> Maybe Value
getNHValueFromWS worldState = getNHValueFromVT (worldNHistory worldState)

getHValueFromVT :: HistoricalTable -> Time -> Idx -> Maybe Value
getHValueFromVT worldHistory time idx =
  IM.lookup time worldHistory >>= (vMapLookup idx . values)

getNHValueFromVT :: NHistoricalTable -> Time -> NKey -> Maybe Value
getNHValueFromVT worldHistory time nKey =
  IM.lookup time worldHistory >>= (vNMapLookup nKey . nValues)

getHValuesFromWS
  :: WorldState -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromWS WorldState {..} = getHValuesFromHT worldHistory

getNHValuesFromWS
  :: WorldState -> [(Time, NKey)] -> [[((Time, NKey), Maybe Value)]]
getNHValuesFromWS WorldState {..} = getNHValuesFromHT worldNHistory

getHValuesFromHT
  :: HistoricalTable -> [(Time, Idx)] -> [[((Time, Idx), Maybe Value)]]
getHValuesFromHT worldHistory indices = map
  (getHValuesFromHTSub worldHistory)
  grouped
 where
  getTime = fst
  sorted  = sortBy (compare `on` getTime) indices
  grouped = groupBy ((==) `on` getTime) sorted

getNHValuesFromHT
  :: NHistoricalTable -> [(Time, NKey)] -> [[((Time, NKey), Maybe Value)]]
getNHValuesFromHT worldHistory indices = map
  (getNHValuesFromHTSub worldHistory)
  grouped
 where
  getTime = fst
  sorted  = sortBy (compare `on` getTime) indices
  grouped = groupBy ((==) `on` getTime) sorted

getHValuesFromHTSub
  :: HistoricalTable -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromHTSub aHTable indices = getHValuesFromHTSubSub aValues indices
 where
  theTime = fst . head $ indices
  aValues = maybe blankVM values (IM.lookup theTime aHTable)

getNHValuesFromHTSub
  :: NHistoricalTable -> [(Time, NKey)] -> [((Time, NKey), Maybe Value)]
getNHValuesFromHTSub aNHTable indices = getNHValuesFromHTSubSub aValues indices
 where
  theTime = fst . head $ indices
  aValues = maybe blankVNM nValues (IM.lookup theTime aNHTable)

getHValuesFromHTSubSub
  :: Values -> [(Time, Idx)] -> [((Time, Idx), Maybe Value)]
getHValuesFromHTSubSub aValues =
  map (\(t, idx) -> ((t, idx), vMapLookup idx aValues))

getNHValuesFromHTSubSub
  :: NValues -> [(Time, NKey)] -> [((Time, NKey), Maybe Value)]
getNHValuesFromHTSubSub aValues =
  map (\(t, nKey) -> ((t, nKey), vNMapLookup nKey aValues))


updateHTValueToWS :: WorldState -> Time -> Idx -> Maybe Value -> WorldState
updateHTValueToWS worldState time idx aMValue = worldState
  { worldHistory = updateValueToHT (worldHistory worldState) time idx aMValue
  }

updateNHTValueToWS :: WorldState -> Time -> NKey -> Maybe Value -> WorldState
updateNHTValueToWS worldState time nKey aMValue = worldState
  { worldNHistory = updateNValueToHT (worldNHistory worldState)
                                     time
                                     nKey
                                     aMValue
  }

updateValueToHT
  :: HistoricalTable -> Time -> Idx -> Maybe Value -> HistoricalTable
updateValueToHT aHistoricalTable time idx aMValue = newHistoricalTable
 where
  baseValues         = maybe blankVM values . IM.lookup time $ aHistoricalTable
  newEpochRow        = EpochRow time (vMapUpdate idx aMValue baseValues)
  newHistoricalTable = IM.insert time newEpochRow aHistoricalTable

updateNValueToHT
  :: NHistoricalTable -> Time -> NKey -> Maybe Value -> NHistoricalTable
updateNValueToHT aNHistoricalTable time nKey aMValue = newNHistoricalTable
 where
  baseNValues = maybe blankVNM nValues . IM.lookup time $ aNHistoricalTable
  newNEpochRow = NEpochRow time (vNMapUpdate nKey aMValue baseNValues)
  newNHistoricalTable = IM.insert time newNEpochRow aNHistoricalTable

updateValuesToWS
  :: WorldState -> [((Time, Idx), Maybe Value)] -> HistoricalTable
updateValuesToWS WorldState {..} = updateValuesToHT worldHistory

updateValuesToHT
  :: HistoricalTable -> [((Time, Idx), Maybe Value)] -> HistoricalTable
updateValuesToHT aHistoricalTable ivList = newHistoricalTable
 where
  getTime            = fst . fst
  sorted             = sortBy (compare `on` getTime) ivList
  grouped            = groupBy ((==) `on` getTime) sorted
  newHistoricalTable = foldr updateValuesToHTSub aHistoricalTable grouped

-- TODO: Should be parallel when update existing element
-- NOTE: But, inserting new element should be serialized
updateValuesToHTSub
  :: [((Time, Idx), Maybe Value)] -> HistoricalTable -> HistoricalTable
updateValuesToHTSub ivList aHistoricalTable = newHistoricalTable
 where
  theTime = fst . fst . head $ ivList
  aEpochRow =
    fromMaybe (EpochRow theTime blankVM) (IM.lookup theTime aHistoricalTable)
  newEpochRow =
    aEpochRow { values = updateValuesToHTSubSub (values aEpochRow) ivList }
  newHistoricalTable = IM.insert theTime newEpochRow aHistoricalTable

updateValuesToHTSubSub :: Values -> [((Time, Idx), Maybe Value)] -> Values
updateValuesToHTSubSub =
  foldr (\((_, idx), aMValue) v -> vMapUpdate idx aMValue v)

updateNValuesToWS
  :: WorldState -> [((Time, NKey), Maybe Value)] -> NHistoricalTable
updateNValuesToWS WorldState {..} = updateNValuesToNHT worldNHistory

updateNValuesToNHT
  :: NHistoricalTable -> [((Time, NKey), Maybe Value)] -> NHistoricalTable
updateNValuesToNHT aNHistoricalTable nvList = newNHistoricalTable
 where
  getTime             = fst . fst
  sorted              = sortBy (compare `on` getTime) nvList
  grouped             = groupBy ((==) `on` getTime) sorted
  newNHistoricalTable = foldr updateNValuesToNHTSub aNHistoricalTable grouped

-- TODO: Should be parallel when update existing element
-- NOTE: But, inserting new element should be serialized
updateNValuesToNHTSub
  :: [((Time, NKey), Maybe Value)] -> NHistoricalTable -> NHistoricalTable
updateNValuesToNHTSub nvList aNHistoricalTable = newNHistoricalTable
 where
  theTime = fst . fst . head $ nvList
  aNEpochRow =
    fromMaybe (NEpochRow theTime blankVNM) (IM.lookup theTime aNHistoricalTable)
  newNEpochRow = aNEpochRow
    { nValues = updateNValuesToNHTSubSub (nValues aNEpochRow) nvList
    }
  newNHistoricalTable = IM.insert theTime newNEpochRow aNHistoricalTable

updateNValuesToNHTSubSub :: NValues -> [((Time, NKey), Maybe Value)] -> NValues
updateNValuesToNHTSubSub =
  foldr (\((_, nKey), aMValue) v -> vNMapUpdate nKey aMValue v)


getDValueFromWS :: WorldState -> Idx -> Maybe Value
getDValueFromWS WorldState {..} = getValueFromValueMap worldDict

getNDValueFromWS :: WorldState -> NKey -> Maybe Value
getNDValueFromWS WorldState {..} = getValueFromValueNMap worldNDict

getVValueFromWS :: WorldState -> Idx -> Maybe Value
getVValueFromWS WorldState {..} = getValueFromValueMap worldVars

getNVValueFromWS :: WorldState -> NKey -> Maybe Value
getNVValueFromWS WorldState {..} = getValueFromValueNMap worldNVars

getValueFromValueMap :: ValueMap -> Idx -> Maybe Value
getValueFromValueMap valueMap idx = IM.lookup idx valueMap

getValueFromValueNMap :: ValueNMap -> NKey -> Maybe Value
getValueFromValueNMap valueNMap nKey = Trie.lookup nKey valueNMap

getDValuesFromWS :: WorldState -> [Idx] -> [(Idx, Maybe Value)]
getDValuesFromWS WorldState {..} = getValuesFromValueMap worldDict

getNDValuesFromWS :: WorldState -> [NKey] -> [(NKey, Maybe Value)]
getNDValuesFromWS WorldState {..} = getValuesFromValueNMap worldNDict

getVValuesFromWS :: WorldState -> [Idx] -> [(Idx, Maybe Value)]
getVValuesFromWS WorldState {..} = getValuesFromValueMap worldVars

getNVValuesFromWS :: WorldState -> [NKey] -> [(NKey, Maybe Value)]
getNVValuesFromWS WorldState {..} = getValuesFromValueNMap worldNVars

getValuesFromValueMap :: ValueMap -> [Idx] -> [(Idx, Maybe Value)]
getValuesFromValueMap valueMap = map (\idx -> (idx, vMapLookup idx valueMap))

getValuesFromValueNMap :: ValueNMap -> [NKey] -> [(NKey, Maybe Value)]
getValuesFromValueNMap valueNMap =
  map (\nKey -> (nKey, vNMapLookup nKey valueNMap))


updateDValueToWS :: WorldState -> Idx -> Maybe Value -> WorldState
updateDValueToWS ws@WorldState {..} idx aMValue =
  ws { worldDict = updateValueToValueMap worldDict idx aMValue }

updateNDValueToWS :: WorldState -> NKey -> Maybe Value -> WorldState
updateNDValueToWS ws@WorldState {..} nKey aMValue =
  ws { worldNDict = updateValueToValueNMap worldNDict nKey aMValue }

updateVValueToWS :: WorldState -> Idx -> Maybe Value -> WorldState
updateVValueToWS ws@WorldState {..} idx aMValue =
  ws { worldVars = updateValueToValueMap worldVars idx aMValue }

updateNVValueToWS :: WorldState -> NKey -> Maybe Value -> WorldState
updateNVValueToWS ws@WorldState {..} nKey aMValue =
  ws { worldNVars = updateValueToValueNMap worldNVars nKey aMValue }

updateValueToValueMap :: ValueMap -> Idx -> Maybe Value -> ValueMap
updateValueToValueMap valueMap idx aMValue = vMapUpdate idx aMValue valueMap

updateValueToValueNMap :: ValueNMap -> NKey -> Maybe Value -> ValueNMap
updateValueToValueNMap valueNMap nKey aMValue =
  vNMapUpdate nKey aMValue valueNMap


updateValuesToDict :: WorldState -> [(Idx, Maybe Value)] -> WorldState
updateValuesToDict ws@WorldState {..} ivList =
  ws { worldDict = updateValuesToValueMap worldDict ivList }

updateValuesToNDict :: WorldState -> [(NKey, Maybe Value)] -> WorldState
updateValuesToNDict ws@WorldState {..} nvList =
  ws { worldNDict = updateValuesToValueNMap worldNDict nvList }

updateValuesToVars :: WorldState -> [(Idx, Maybe Value)] -> WorldState
updateValuesToVars ws@WorldState {..} ivList =
  ws { worldVars = updateValuesToValueMap worldVars ivList }

updateValuesToNVars :: WorldState -> [(NKey, Maybe Value)] -> WorldState
updateValuesToNVars ws@WorldState {..} nvList =
  ws { worldNVars = updateValuesToValueNMap worldNVars nvList }

updateValuesToValueMap :: ValueMap -> [(Idx, Maybe Value)] -> ValueMap
updateValuesToValueMap = foldr (\(i, mV) -> vMapUpdate i mV)

updateValuesToValueNMap :: ValueNMap -> [(NKey, Maybe Value)] -> ValueNMap
updateValuesToValueNMap = foldr (\(n, mV) -> vNMapUpdate n mV)
