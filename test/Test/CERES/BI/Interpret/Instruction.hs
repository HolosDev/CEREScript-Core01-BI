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

case_InitVariable = assertEqual
  "crsInitVariable (blankWorld, undefined, blankEnv) (VP AtDict (VII 1)) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\")))"
  answer
  question
 where
  question = crsInitVariable (blankWorld, undefined, blankEnv)
                             (VP AtDict (VII 1))
                             (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton 1 (W (Just (StrValue "AtDict[VII=1]")))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_SetValue = assertEqual
  "crsSetValue (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\")))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv = crsInitVariable iInput
                           (VP AtDict (VII 1))
                           (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
  question = crsSetValue (blankWorld, undefined, theEnv)
                         (VP AtDict (VII 1))
                         (VP AtHere (VIV (IntValue 1)))
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton 1 (W (Just (IntValue 1)))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_DeleteVariable = assertEqual
  "crsDeleteVariable (blankWorld, undefined, theEnv) (VP AtDict (VII 1))"
  answer
  question
 where
  iInput                  = (blankWorld, undefined, blankEnv)
  theEnv@(theWC, _, _, _) = crsInitVariable
    iInput
    (VP AtDict (VII 1))
    (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
  theWorld       = worldCacheCommitter theWC (worldState blankWorld)
  (nWC, _, _, _) = crsSetValue (blankWorld, undefined, theEnv)
                               (VP AtDict (VII 1))
                               (VP AtHere (VIV (IntValue 1)))
  question = worldCacheCommitter nWC theWorld
  answer   = worldState blankWorld

case_Modify1 = assertEqual
  "crsModifyValue1 (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) COANeg"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv =
    crsInitVariable iInput (VP AtDict (VII 1)) (VP AtHere (VIV (IntValue 1)))
  question =
    crsModifyValue1 (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) COANeg
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton 1 (W (Just (IntValue (-1))))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_Modify2 = assertEqual
  "crsModifyValue2 (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) (VP AtDict (VII 2)) COASub"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv1 =
    crsInitVariable iInput (VP AtDict (VII 1)) (VP AtHere (VIV (IntValue 1)))
  theInput1 = (blankWorld, undefined, theEnv1)
  theEnv2 =
    crsInitVariable theInput1 (VP AtDict (VII 2)) (VP AtHere (VIV (IntValue 3)))
  question = crsModifyValue2 (blankWorld, undefined, theEnv2)
                             (VP AtDict (VII 1))
                             (VP AtDict (VII 2))
                             COASub
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.fromList [(1, W (Just (IntValue (-2)))), (2, W (Just (IntValue 3)))]
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_CopyValue = assertEqual
  "crsCopyValue (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\")))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv = crsInitVariable iInput
                           (VP AtDict (VII 1))
                           (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
  question = crsCopyValue (blankWorld, undefined, theEnv)
                          (VP AtDict (VII 1))
                          (VP AtHere (VIV (IntValue 1)))
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton 1 (W (Just (IntValue 1)))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_SetVPosition = assertEqual
  "crsSetVPosition (blankWorld, undefined, blankEnv) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  question = crsSetVPosition (blankWorld, undefined, blankEnv)
                             (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
                             (VP AtDict (VII 1))
  answer =
    ( ( IM.empty
      , IM.empty
      , blankVM
      , blankVNM
      , IM.singleton
        1
        (W (Just (PtrValue (VP AtHere (VIV (StrValue "AtDict[VII=1]"))))))
      , blankVNM
      )
    , blankLocalCache
    , blankTrickCache
    , blankRG
    )

case_GetVPosition = assertEqual
  "crsGetVPosition (blankWorld, undefined, blankEnv) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  question = crsGetVPosition (blankWorld, undefined, blankEnv)
                             (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
                             (VP AtDict (VII 1))
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
