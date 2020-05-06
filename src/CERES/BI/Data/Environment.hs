module CERES.BI.Data.Environment where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Trie.Text                 ( Trie )

import           TextShow


import           Data.CERES.Data
import           Data.CERES.Data.Error

import           CERES.BI.Type

import           CERES.BI.Util.Random


-- TODO: How to distinguish
--  * 1. Referred, Added, or Changed
--  * 2. Referred, but deleted

-- TODO: Value -> ValueContainer
type RWMV = RW (Maybe Value)
type RWMVMap = IntMap RWMV
type RWMVNMap = Trie RWMV

type WorldCache
  = ( HistoricalCache
    , NHistoricalCache
    , DictionaryCache
    , NDictionaryCache
    , VariableCache
    , NVariableCache
    )
-- | HistoricalCache (Map Time (Map ID (Maybe Value)))
type HistoricalCache = IntMap RWMVMap
type NHistoricalCache = IntMap RWMVNMap
type DictionaryCache = RWMVMap
type NDictionaryCache = RWMVNMap
type VariableCache = RWMVMap
type NVariableCache = RWMVNMap

type LocalCache = (LocalVariables, LocalNVariables, LocalTemp, LocalNTemp)
type LocalVariables = ValueMap
type LocalNVariables = ValueNMap
type LocalTemp = ValueMap
type LocalNTemp = ValueNMap

type TrickCache = ValueNMap

type Env = (WorldCache, LocalCache, TrickCache, RG)
blankEnv = (blankWorldCache, blankLocalCache, blankTrickCache, blankRG)
blankWorldCache = (IM.empty, IM.empty, blankVM, blankVNM, blankVM, blankVNM)
blankLocalCache = (blankVM, blankVNM, blankVM, blankVNM)
blankTrickCache = blankVNM
blankRG = mkGenFromInt 0

data RW a = R a | W a | RW a deriving (Eq, Ord)

instance (Show a, TextShow a) => Show (RW a) where
  show = toString . showb

instance TextShow a => TextShow (RW a) where
  showb (R  a) = fromText "[R  " <> showb a <> fromText "]"
  showb (W  a) = fromText "[W  " <> showb a <> fromText "]"
  showb (RW a) = fromText "[RW " <> showb a <> fromText "]"

runRW :: RW a -> a
runRW (R  a) = a
runRW (W  a) = a
runRW (RW a) = a

notR :: RW a -> Bool
notR (R _) = False
notR _     = True
