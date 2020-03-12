module CERES.BI.Data.Environment where


import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Maybe

import           TextShow

import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value
import           Data.CERES.Value.Error

import           CERES.BI.Type


-- TODO: How to distinguish
--  * 1. Referred, Added, or Changed
--  * 2. Referred, but deleted

-- TODO: Value -> ValueContainer
type RWMV = RW (Maybe Value)
type RWMVMap = IntMap RWMV

type WorldCache = (HistoricCache, DictionaryCache, VariableCache)
-- | HistoricCache (Map Time (Map ID (Maybe Value)))
type HistoricCache = IntMap RWMVMap
type DictionaryCache = RWMVMap
type VariableCache = RWMVMap

type LocalVariables = ValueMap
type LocalCache = ValueMap

type Env = (WorldCache, LocalVariables, LocalCache, RG)

data RW a = R a | W a | RW a deriving (Eq, Ord)

instance (Show a, TextShow a) => Show (RW a) where
  show = TL.unpack . showtl

instance TextShow a => TextShow (RW a) where
  showb (R  a) = fromLazyText "[R  " <> showb a <> fromLazyText "]"
  showb (W  a) = fromLazyText "[W  " <> showb a <> fromLazyText "]"
  showb (RW a) = fromLazyText "[RW " <> showb a <> fromLazyText "]"

runRW :: RW a -> a
runRW (R  a) = a
runRW (W  a) = a
runRW (RW a) = a

notR :: RW a -> Bool
notR (R _) = False
notR _     = True
