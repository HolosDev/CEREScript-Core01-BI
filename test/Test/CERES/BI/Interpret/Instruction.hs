module Test.CERES.BI.Interpret.Instruction where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import           Data.Bifunctor
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL

import           TextShow


import           CERES.Operate

import           Data.CERES.Data
import           Data.CERES.Data.Method
import           Data.CERES.Operator
import           Data.CERES.Parser
import           Data.CERES.Type
import           Data.CERES.Variable.Parser

import           CERES.BI.Data
import           CERES.BI.Data.Cache.Function
import           CERES.BI.Data.Constants
import           CERES.BI.Data.Environment
import           CERES.BI.Data.Function

import           CERES.BI.Interpret.Cache
import           CERES.BI.Interpret.Instruction
import           CERES.BI.Interpret.Spool

import           CERES.BI.Type

import           CERES.BI.Util.Random

import           Debug

tests = $(testGroupGenerator)

case_nothing = assertBool "Nothing" True

case_err01 = assertEqual "crsGetVPosition blankWorld undefined blankEnv (VP AtHere (VIV (StrValue \"AtDict[VII=1]\"))) (VP AtDict (VII 1))" question answer
 where
  question = crsGetVPosition blankWorld undefined blankEnv (VP AtHere (VIV (StrValue "AtDict[VII=1]"))) (VP AtDict (VII 1))
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton 1 (W (Just (PtrValue (VP AtDict (VII 1)))))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )
