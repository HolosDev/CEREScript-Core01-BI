module CERES.BI.Type
  ( module CERES.BI.Type
  , CERES.BI.Util.Random.RG
  , CERES.BI.Util.Random.GSeed
  )
where


import           Data.Maybe
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Trie.Text                 ( Trie )
import qualified Data.Trie.Text                as Trie
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HM


import           Data.CERES.Data
import           Data.CERES.Type

-- NOTE: Alias for abstract PRNG type
import           CERES.BI.Util.Random


data Maker s f a = Maker { mDef :: a, mF :: f, mMaker :: s -> f -> Maybe a }

runMaker Maker {..} s = fromMaybe mDef (mMaker s mF)

type InternalTime = Int


type ValueMap = IntMap Value
type ValueNMap = Trie Value
type ValueNHMap = HashMap NKey Value

blankVM = IM.empty
blankVNM = Trie.empty
blankVNHM = HM.empty

-- | ValueList remembers type of each Variable
type ValueList = IntMap ValueTyper
