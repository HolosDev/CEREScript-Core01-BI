module CERES.BI.Data where


import           Data.Function
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Trie.Text                 ( Trie )
import qualified Data.Trie.Text                as Trie

import           TextShow


import           Data.CERES.Data
import           Data.CERES.Type

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
  , worldHistory  :: HistoricalTable
  , worldNHistory :: NHistoricalTable
  , worldDict     :: Dictionary
  , worldNDict    :: NDictionary
  , worldVars     :: Variables
  , worldNVars    :: NVariables
  , worldRG       :: RG
  } deriving Show

type TimeSpan = Maybe (Time, Time)
type HistoricalTable = IntMap EpochRow
type NHistoricalTable = IntMap NEpochRow
data EpochRow = EpochRow
  { eRowTime :: {-# UNPACK #-} !Time
  , values   :: Values
  } deriving Show
data NEpochRow = NEpochRow
  { nERowTime :: {-# UNPACK #-} !Time
  , nValues   :: NValues
  } deriving Show

type Values = ValueMap
type NValues = ValueNMap
type Dictionary = Values
type NDictionary = NValues
type Variables = Values
type NVariables = NValues

-- | Spools contains every spool code
type Spools = IntMap Spool
type Spool = CERESSpool

data CERESSpool = CERESSpool
  { csID              :: {-# UNPACK #-} !ID -- NOTE: ID of Spool code, not instance
  , csName            :: Name
  , csScript          :: Maker (World,Env) CEREScript CEREScript
  , csSINameMaker     :: Maker (World,Env) CEREScript Name
  , csSIReadVPMaker   :: Maker (World,Env) CEREScript (Set VPosition) -- TODO: Not sure this could be static or dynamic
  , csSIWriteVPMaker  :: Maker (World,Env) CEREScript (Set VPosition) -- TODO: Not sure this could be static or dynamic
  , csSIPriorityMaker :: Maker (World,Env) CEREScript Priority
  , csInitLocalVars   :: ValueMap
  , csInitLocalNVars  :: ValueNMap
  , csInitLocalTemp   :: ValueMap
  , csInitLocalNTemp  :: ValueNMap
  }

instance Eq CERESSpool where
  (==) = (==) `on` csID

instance Ord CERESSpool where
  compare = compare `on` csID

instance Show CERESSpool where
  show = toString . showb

instance TextShow CERESSpool where
  showb CERESSpool {..} =
    fromText "Spool(" <> showb csID <> fromText "): " <> fromText csName

data SpoolInstanceRow = SIRow
  { siRowTime :: Time
  , sis       :: SpoolInstances
  } deriving Show

type SpoolInstances = IntMap SpoolInstance

data SpoolInstance = SI
  { siID         :: {-# UNPACK #-} !ID
  , siName       :: Name
  , siPriority   :: {-# UNPACK #-} !Priority
  , siRWVPSet    :: Set VPosition -- Only World, Dict, Var
  , siLocalVars  :: LocalVariables
  , siLocalNVars :: LocalNVariables
  , siSpoolID    :: {-# UNPACK #-} !ID
  , siRestScript :: CEREScript
  , siRG         :: RG
  , siF          :: World -> World
  }

instance Eq SpoolInstance where
  (==) = (==) `on` siID

instance Ord SpoolInstance where
  compare siA siB = if pCompared == EQ
    then (compare `on` siID) siA siB
    else pCompared
   where
    pCompared :: Ordering
    pCompared = (compare `on` siPriority) siA siB

instance Show SpoolInstance where
  show = toString . showb

instance TextShow SpoolInstance where
  showb SI {..} =
    fromText "SI("
      <> showb siID
      <> fromText "): "
      <> fromText siName
      <> fromText " <"
      <> showb siPriority
      <> fromText "> Based on Spool("
      <> showb siSpoolID
      <> ")"

type SIIS = SpoolInstanceInheritStatus
-- NOTE: SIJump takes relative time-slot
data SpoolInstanceInheritStatus = SIJump Int | SIEnd
