module CERES.BI.Interpret.Modify where


import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           TextShow


import           CERES.Operate

import           Data.CERES.Data
import           Data.CERES.Data.Method
import           Data.CERES.Operator
import           Data.CERES.Type

import           Data.CERES.Parser
import           Data.CERES.Variable.Parser


import           CERES.BI.Data
import           CERES.BI.Data.Cache.Function

import           Debug
import           Debug.Trace

modify1 :: CERESOperator -> Value -> Value
modify1 aOp aValue = newValue
 where
  mOperator = operator1Selector aOp
  newValue  = maybe
    (ErrValue $ "[ERROR]<modify1> No such operator1 like " <> showt aOp)
    (\o -> o aValue)
    mOperator

modify2 :: CERESOperator -> Value -> Value -> Value
modify2 aOp vA vB = newValue
 where
  mOperator = operator2Selector aOp
  newValue  = maybe
    (ErrValue $ "[ERROR]<modify2> No such operator1 like " <> showt aOp)
    (\o -> o vA vB)
    mOperator

modify3 :: CERESOperator -> Value -> Value -> Value -> Value
modify3 aOp vA vB vC = newValue
 where
  mOperator = undefined -- operator3Selector aOp
  newValue  = maybe
    (ErrValue $ "[ERROR]<modify1> No such operator3 like " <> showt aOp)
    (\o -> o vA vB vC)
    mOperator

modify4 :: CERESOperator -> Value -> Value -> Value -> Value -> Value
modify4 aOp vA vB vC vD = newValue
 where
  mOperator = undefined -- operator4Selector aOp
  newValue  = maybe
    (ErrValue $ "[ERROR]<modify1> No such operator4 like " <> showt aOp)
    (\o -> o vA vB vC vD)
    mOperator

{-
NOTE: Return type
Return value is consisted of Non-replacing Text and VariablePosition Text
[NRT, VPT, NRT, VPT ...]
-}
findVariables :: Str -> Either Message [Str]
findVariables given = findVariablesSub given []
findVariablesSub given accList = if T.null postStr
  then Right $ preStr : accList
  else if T.isPrefixOf "}" restStr
    then findVariablesSub (T.drop 1 restStr) (stripped : preStr : accList)
    else Left $ "[ERROR]<findVariable> Wrong Syntax " <> given
 where
  (preStr  , postStr) = T.breakOn "${" given
  (stripped, restStr) = T.breakOn "}" . T.drop 2 $ postStr

-- FIXME: Should be redesigned with other parser and Either return type
replaceStr :: Input -> [Str] -> Str
replaceStr anInput aStrList = T.concat $ replaceStrNormal [] aStrList
 where
  replaceStrNormal accList []            = accList
  replaceStrNormal accList (nStr : strs) = replaceStrVP (nStr : accList) strs
  replaceStrVP accList []                  = accList
  replaceStrVP accList (shortVPStr : strs) = replaceStrNormal
    (aStr : accList)
    strs
   where
    (storage : indices) = T.split (== ':') shortVPStr
    -- TODO: add partial key support like TrP
    aVP                 = case storage of
      "Tr"  -> VP AtTricky parseNIndices
      "Ptr" -> VP AtPtr (error "ZZZZ")
      "W"   -> VP AtWorld parseIIndices
      "NW"  -> VP AtNWorld parseNIndices
      "T"   -> VP AtTime parseIIndices
      "NT"  -> VP AtNTime parseNIndices
      "D"   -> VP AtDict parseIIndices
      "ND"  -> VP AtNDict parseNIndices
      "V"   -> VP AtVars parseIIndices
      "NV"  -> VP AtNVars parseNIndices
      "LV"  -> VP AtLVars parseIIndices
      "LNV" -> VP AtLNVars parseNIndices
      "LT"  -> VP AtLTemp parseIIndices
      "LNT" -> VP AtLNTemp parseNIndices
      "R"   -> VP AtReg parseIIndices
      "H"   -> VP
        AtHere
        ( VIV
        . fromJust
        . getResult
        . parseValue
        . TL.fromStrict
        $ (indices !! 0)
        )
      "N" -> VP AtNull VINull
    idxLen        = length indices
    parseNIndices = case idxLen of
      1 -> VIN (indices !! 0)
      2 -> if isNHT
        then (VINT (indices !! 0) (readInt' (indices !! 1)))
        else error "XXXX"
      _ -> error "[ERROR]<replaceStr :=: parseNIndices> Not yet supported"
    parseIIndices = case idxLen of
      1 -> VII (readInt' (indices !! 0))
      2 -> if isHT
        then (VIIT (readInt' (indices !! 0)) (readInt' (indices !! 1)))
        else error "YYYY"
      _ -> error "[ERROR]<replaceStr :=: parseIIndices> Not yet supported"
    isHT     = storage == "T" || storage == "W"
    isNHT    = storage == "NT" || storage == "NW"
    readInt' = fst . fromJust . readInt . TL.fromStrict
    aStr     = getStr $ getValue anInput aVP
