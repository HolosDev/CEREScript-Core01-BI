module CERES.BI.Interpret.SpoolInstance where


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
import           CERES.BI.Type


-- # elapseTime
-- ## Set SIStatus for Jumping

elapseTime :: InternalTime -> SIStatus -> SIStatus
elapseTime worldTSSize aSIS@(aSIStatus, aSI) =
  (SIParams (SIJump jumpTarget), theSI)
 where
  (jumpTarget, truncatedSIElapsedTime) = quotRem (siElapsedTime aSI) worldTSSize
  theSI = aSI { siElapsedTime = truncatedSIElapsedTime }
