module CERES.BI.Util where


import           Data.IntMap                   as IM
import           Data.Trie.Text                as Trie

import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Type


vMapInsert :: Idx -> Value -> ValueMap -> ValueMap
vMapInsert = IM.insert
vNMapInsert :: NKey -> Value -> ValueNMap -> ValueNMap
vNMapInsert = Trie.insert

vMapUpdate :: Idx -> Maybe Value -> ValueMap -> ValueMap
vMapUpdate idx mValue = IM.update (const mValue) idx
vNMapUpdate :: NKey -> Maybe Value -> ValueNMap -> ValueNMap
vNMapUpdate nKey mValue vnMap =
  maybe (Trie.delete nKey vnMap) (\v -> Trie.insert nKey v vnMap) mValue

vMapLookup :: Idx -> ValueMap -> Maybe Value
vMapLookup = IM.lookup
vNMapLookup :: NKey -> ValueNMap -> Maybe Value
vNMapLookup = Trie.lookup
