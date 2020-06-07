module CERES.BI.Interpret.SpoolTree where


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
  { stSize :: Int
  , siList :: [SpoolInstance]
  , vpSet :: Set VPosition
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
    | null jList       = SpoolTree 1 [aSI] $! siRWVPSet
    | length jList > 1 = SpoolTree (jSize + 1) (aSI : jSIList) $! jRWVPSet
    | otherwise        = SpoolTree (oSize + 1) (aSI : oSIList) $! oRWVPSet
   where
    SpoolTree oSize oSIList oVPSet = head jList
    SpoolTree jSize jSIList jVPSet =
      foldr stJoiner (SpoolTree 0 [] S.empty) dList
    stJoiner stA stB = SpoolTree (stSize stA + stSize stB)
                                 (siList stA <> siList stB)
                                 (S.union (vpSet stA) (vpSet stB))
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
