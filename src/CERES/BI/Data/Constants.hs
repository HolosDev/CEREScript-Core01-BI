module CERES.BI.Data.Constants where


import           Data.CERES.Data
import           Data.CERES.Type

import           CERES.BI.Data


indexTimeToEnd :: Idx
indexTimeToEnd = 0


-- WorldVars
wsSimulationControlIdx :: Idx
wsSimulationControlIdx = 0

-- LocalVariables
cf0Idx, cf1Idx, executingTimeIdx, elapsedInternalTimeIdx, resumeCodeIdx :: Idx
cf0Idx = -1
cf1Idx = -2
executingTimeIdx = -3
elapsedInternalTimeIdx = -4
resumeCodeIdx = -5

-- LocalCache
retainCodeIdx, jumpOffsetIdx, spCodeIdx :: Idx
retainCodeIdx = -1
jumpOffsetIdx = -2
spCodeIdx = -4
