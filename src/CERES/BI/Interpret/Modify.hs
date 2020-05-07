module CERES.BI.Interpret.Modify where


import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           TextShow


import           CERES.Operate

import           Data.CERES.Data
import           Data.CERES.Operator
import           Data.CERES.Type

import           Debug
import           Debug.Trace

modify1 :: CERESOperator -> Value -> Value
modify1 aOp aValue = newValue
 where
  mOperator = operator1Selector aOp
  newValue  = maybe
    (ErrValue $ T.append "[ERROR]<modify1> No such operator1 like " (showt aOp))
    (\o -> o aValue)
    mOperator

modify2 :: CERESOperator -> Value -> Value -> Value
modify2 aOp vA vB = newValue
 where
  mOperator = operator2Selector aOp
  newValue  = maybe
    (ErrValue $ T.append "[ERROR]<modify2> No such operator1 like " (showt aOp))
    (\o -> o vA vB)
    mOperator

modify3 :: CERESOperator -> Value -> Value -> Value -> Value
modify3 aOp vA vB vC = newValue
 where
  mOperator = undefined -- operator3Selector aOp
  newValue  = maybe
    (ErrValue $ T.append "[ERROR]<modify1> No such operator3 like " (showt aOp))
    (\o -> o vA vB vC)
    mOperator

modify4 :: CERESOperator -> Value -> Value -> Value -> Value -> Value
modify4 aOp vA vB vC vD = newValue
 where
  mOperator = undefined -- operator4Selector aOp
  newValue  = maybe
    (ErrValue $ T.append "[ERROR]<modify1> No such operator4 like " (showt aOp))
    (\o -> o vA vB vC vD)
    mOperator
