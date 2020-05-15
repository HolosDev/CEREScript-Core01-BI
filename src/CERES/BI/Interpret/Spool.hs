module CERES.BI.Interpret.Spool where


import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( partition )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment


type SpoolForest = [SpoolTree]
data SpoolTree = SpoolTree
  { vpSet :: Set VPosition
  , siList :: [SpoolInstance]
  }

siAggregator :: World -> SpoolForest
siAggregator World {..} = IM.foldr siAggregatorSub [] theSIs
  where theSIs = maybe IM.empty sis . IM.lookup worldTime $ worldSITable

-- NOTE: Part spoolTrees as (HaveJoint, Disjoint)
-- TODO: Need Eq instance which could take care of AtWorld - AtTime with WorldTime
siAggregatorSub :: SpoolInstance -> SpoolForest -> SpoolForest
siAggregatorSub aSI@SI {..} aSpoolForest = newSpoolTree : dList
 where
  (dList, jList) = partition (S.disjoint siRWVPSet . vpSet) aSpoolForest
  newSpoolTree
    | null jList       = siRWVPSet `seq` SpoolTree siRWVPSet [aSI]
    | length jList > 1 = jRWVPSet `seq` SpoolTree jRWVPSet (aSI : jSIList)
    | otherwise        = oRWVPSet `seq` SpoolTree oRWVPSet (aSI : oSIList)
   where
    SpoolTree oVPSet oSIList = head jList
    SpoolTree jVPSet jSIList = foldr stJoiner (SpoolTree S.empty []) dList
    stJoiner stA stB =
      SpoolTree (S.union (vpSet stA) (vpSet stB)) (siList stA ++ siList stB)
    jRWVPSet = S.union jVPSet siRWVPSet
    oRWVPSet = S.union oVPSet siRWVPSet

siisExecutor
  :: Time
  -> SpoolInstanceTable
  -> [(SIParams, SpoolInstance)]
  -> SpoolInstanceTable
siisExecutor worldTime theSITable siPList = foldr siisExecutorSub
                                                  theSITable
                                                  siPList
 where
  siisExecutorSub (SIParams (SIJump offset), si) aSITable = newSITable
   where
    theTime    = worldTime + offset
    targetSIs  = maybe IM.empty sis . IM.lookup theTime $ aSITable
    newSIRow   = SIRow theTime . IM.insert theTime si $ targetSIs
    newSITable = IM.insert theTime newSIRow theSITable
  siisExecutorSub _ aSITable = aSITable
