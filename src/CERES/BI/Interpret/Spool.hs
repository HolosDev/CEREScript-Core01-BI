module CERES.BI.Interpret.Spool where


import           CERES.Operate
import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data
import           CERES.BI.Data.Constants

import           CERES.BI.Data.Environment

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( partition )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S


type SpoolForest = [SpoolTree]
data SpoolTree = SpoolTree
  { vpSet :: Set VPosition
  , siList :: [SpoolInstance]
  }

siAggregator :: World -> [SpoolTree]
siAggregator World {..} = undefined
 where
  theSIs    = maybe IM.empty sis . IM.lookup worldTime $ worldSITable
  spoolTree = IM.foldr siAggregatorSub [] theSIs

-- NOTE: Part spoolTrees as (HaveJoint, Disjoint)
-- TODO: Need Eq instance which could take care of AtWorld - AtTime with WorldTime
siAggregatorSub :: SpoolInstance -> SpoolForest -> SpoolForest
siAggregatorSub aSI@SI {..} aSpoolForest = newSpoolTree : dList
 where
  (dList, jList) = partition (S.disjoint siVPS . vpSet) aSpoolForest
  newSpoolTree
    | null jList       = SpoolTree siVPS [aSI]
    | length jList > 1 = SpoolTree (S.union jVPSet siVPS) (aSI : jSIList)
    | otherwise        = SpoolTree (S.union oVPSet siVPS) (aSI : oSIList)
   where
    SpoolTree oVPSet oSIList = head jList
    SpoolTree jVPSet jSIList = foldr stJoiner (SpoolTree S.empty []) dList
    stJoiner stA stB =
      SpoolTree (S.union (vpSet stA) (vpSet stB)) (siList stA ++ siList stB)
