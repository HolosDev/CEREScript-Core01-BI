module CERES.BI.Data where

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import           Data.Trie.Text                 ( Trie )
import qualified Data.Trie.Text                as Trie

import           TextShow

import           Data.CERES.Script
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data.Environment
import           CERES.BI.Type


-- | World stores everything
data World = World
  { worldSpools    :: Spools
  , worldValueList :: ValueList
  , worldState     :: WorldState
  , worldSITable   :: SpoolInstanceTable
  , worldTime      :: {-# UNPACK #-} !Time
  , worldTSSize    :: {-# UNPACK #-} !InternalTime
  } deriving Show

-- | ValueList remembers type of each Variable
type ValueList = IntMap ValueTyper
type SpoolInstanceTable = IntMap SpoolInstanceRow

-- No Branch World yet
data WorldState = WorldState
  { evaluatedSpan :: TimeSpan
  , worldHistory  :: HistoricTable
  , worldDict     :: Dictionary
  , worldNDic     :: NDictionary
  , worldVars     :: Variables
  , worldRG       :: RG
  } deriving Show

type TimeSpan = Maybe (Time, Time)
type HistoricTable = IntMap EpochRow
data EpochRow = EpochRow
  { valueRowTime :: {-# UNPACK #-} !Time
  , values       :: Values
  } deriving Show
type Values = ValueMap
type Dictionary = ValueMap
type NDictionary = Trie Value
type Variables = ValueMap

-- | Spools contains every spool code
type Spools = IntMap Spool
type Spool = CERESSpool

data CERESSpool = CERESSpool
  { csID       :: {-# UNPACK #-} !ID -- NOTE: ID of Spool code, not instance
  , csName     :: {-# UNPACK #-} !Name
  , csScript   :: CEREScript
  -- TODO: Not sure this could be static or dynamic
  , readVP     :: Set VPosition
  , writeVP    :: Set VPosition
  , csPriority :: {-# UNPACK #-} !Priority
  , csInitLocalVars :: ValueMap
  , csInitLocalCache :: ValueMap
  } deriving (Eq, Ord, Show, Read)

data SpoolInstanceRow = SIRow
  { siRowTime :: Time
  , sis       :: SpoolInstances
  } deriving Show

type SpoolInstances = IntMap SpoolInstance

data SpoolInstance = SI
  { siID         :: {-# UNPACK #-} !ID
  , siName       :: {-# UNPACK #-} !Name
  , siVPS        :: Set VPosition -- Only World, Dict, Var
  , siLocalVars  :: LocalVariables
  , siSpoolID    :: {-# UNPACK #-} !ID
  , siRestScript :: CEREScript
  , siRG         :: RG
  , siF          :: World -> World
  }

instance Show SpoolInstance where
  show = TL.unpack . showtl

instance TextShow SpoolInstance where
  showb (SI id name _ _ sID _ _ _) =
    fromLazyText "SI("
      <> showb id
      <> fromLazyText "): "
      <> fromLazyText name
      <> fromLazyText " Based on Spool("
      <> showb sID
      <> ")"

type SIIS = SpoolInstanceInheritStatus
-- NOTE: SIJump takes relative time-slot
data SpoolInstanceInheritStatus = SIJump Int | SIEnd
