module CERES.BI.Data.Constants where


import           Data.CERES.Script
import           Data.CERES.Operator
import           Data.CERES.Type
import           Data.CERES.Value

import           CERES.BI.Data

indexTimeToEnd :: ID
indexTimeToEnd = 0


-- WorldVars
wsSimulationControlID :: ID
wsSimulationControlID = 0

-- LocalVariables
cf0ID, cf1ID, executingTimeID, elapsedInternalTimeID, resumeCodeID :: ID
cf0ID = 0
cf1ID = 1
executingTimeID = 2
elapsedInternalTimeID = 3
resumeCodeID = 4

-- LocalCache
retainCodeID, jumpOffsetID, spCodeID :: ID
retainCodeID = 0
jumpOffsetID = 1
spCodeID = 4
