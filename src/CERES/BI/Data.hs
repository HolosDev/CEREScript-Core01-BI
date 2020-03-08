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

import           TextShow

import           Data.CERES.Script
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data.Environment


-- | World stores everything
data World = World
  { worldSpools    :: Spools
  , worldValueList :: ValueList
  , worldState     :: WorldState
  , worldSITable   :: SpoolInstanceTable
  , worldTime      :: Time
  } deriving Show

-- | ValueList remembers type of each Variable
type ValueList = IntMap ValueTyper
type SpoolInstanceTable = IntMap SpoolInstanceRow

-- No Branch World yet
data WorldState = WorldState
  { evaluatedSpan :: TimeSpan
  , worldHistory  :: HistoricTable
  , worldDict     :: Dictionary
  , worldVars     :: Variables
  } deriving Show

type TimeSpan = Maybe (Time, Time)
type HistoricTable = IntMap EpochRow
data EpochRow = EpochRow
  { valueRowTime :: Time
  , values       :: Values
  } deriving Show
type Values = ValueMap
type Dictionary = ValueMap
type Variables = ValueMap

-- | Spools contains every spool code
type Spools = IntMap Spool
type Spool = CERESSpool

data CERESSpool = CERESSpool
  { csID       :: ID -- NOTE: ID of Spool code, not instance
  , csName     :: Name
  , csScript   :: CEREScript
  -- TODO: Not sure this could be static or dynamic
  , readVP     :: Set VPosition
  , writeVP    :: Set VPosition
  , csPriority :: Priority
  } deriving (Eq, Ord, Show, Read)

data SpoolInstanceRow = SIRow
  { siRowTime :: Time
  , sis       :: SpoolInstances
  } deriving Show

type SpoolInstances = IntMap SpoolInstance

data SpoolInstance = SI
  { siID         :: ID
  , siName       :: Name
  , siVPS        :: Set VPosition -- Only World, Dict, Var
  , siLocalVars  :: LocalVariables
  , siSpoolID    :: ID
  , siRestScript :: CEREScript
  , siF          :: World -> World
  }

instance Show SpoolInstance where
  show = TL.unpack . showtl

instance TextShow SpoolInstance where
  showb (SI id name _ _ sID _ _) =
    fromLazyText "SI("
      <> showb id
      <> fromLazyText "): "
      <> fromLazyText name
      <> fromLazyText " Based on Spool("
      <> showb sID
      <> ")"

type SIIS = SpoolInstanceInheritStatus
data SpoolInstanceInheritStatus = SIJump Int | SIEnd
