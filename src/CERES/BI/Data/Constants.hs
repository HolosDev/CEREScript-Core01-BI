module CERES.BI.Data.Constants where


import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data


indexTimeToEnd :: Idx
indexTimeToEnd = 0


-- WorldVars
wsSimulationControlIdx :: Idx
wsSimulationControlIdx = 0

-- LocalVariables
cf0Idx, cf1Idx, executingTimeIdx, elapsedInternalTimeIdx, resumeCodeIdx :: Idx
cf0Idx = 0
cf1Idx = 1
executingTimeIdx = 2
elapsedInternalTimeIdx = 3
resumeCodeIdx = 4

-- LocalCache
retainCodeIdx, jumpOffsetIdx, spCodeIdx :: Idx
retainCodeIdx = 0
jumpOffsetIdx = 1
spCodeIdx = 4
