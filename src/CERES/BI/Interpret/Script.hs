module CERES.BI.Interpret.Script where


import           Data.CERES.Data
import           Data.CERES.Data.Method

import           CERES.BI.Data
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function
import           CERES.BI.Util


isScriptEnd :: CEREScript -> Bool
isScriptEnd = null

-- NOTE: exceedTime
-- * Calculate theElapsedTime exceeds worldTSSize or not
-- * Returns whether a sum of theElapsedTime and SI-ElapsedTime exceeds or not
exceedTS :: Input -> Bool
exceedTS anInput@(aWorld@World {..}, aSI@SI {..}, cState) =
   theElapsedTime + siElapsedTime > worldTSSize
 where
  theElapsedTime = maybe 0 getInt $ vMapLookup elapsedInternalTimeIdx (lVars . lCache $ cState)
