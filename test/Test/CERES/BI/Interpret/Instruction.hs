module Test.CERES.BI.Interpret.Instruction where


import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit.Base

import           Data.Bifunctor
import qualified Data.HashMap.Strict           as HM
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
    setDCache (IM.singleton 1 (W (Just (StrValue "AtDict[VII=1]")))) blankEnv

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
  answer = setDCache (IM.singleton 1 (W (Just (IntValue 1)))) blankEnv

case_DeleteVariable = assertEqual
  "crsDeleteVariable (blankWorld, undefined, theEnv) (VP AtDict (VII 1))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv = crsInitVariable iInput
                           (VP AtDict (VII 1))
                           (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
  theState = worldCacheCommitter (wCache theEnv) (worldState blankWorld)
  newEnv   = crsSetValue
    (blankWorld { worldState = theState }, undefined, blankEnv)
    (VP AtDict (VII 1))
    (VP AtHere (VIV (IntValue 1)))
  question = worldCacheCommitter (wCache newEnv) theState
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
  answer = setDCache (IM.singleton 1 (W (Just (IntValue (-1))))) blankEnv

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
  answer = setDCache
    (IM.fromList [(1, W (Just (IntValue (-2)))), (2, W (Just (IntValue 3)))])
    blankEnv

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
  answer = setDCache (IM.singleton 1 (W (Just (IntValue (1))))) blankEnv

case_ConvertValue = assertEqual
  "crsConvertValue (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) VTDbl"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv =
    crsInitVariable iInput (VP AtDict (VII 1)) (VP AtHere (VIV (IntValue 12)))
  theInput = (blankWorld, undefined, theEnv)
  question =
    crsConvertValue (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) VTDbl
  answer = setDCache (IM.singleton 1 (W (Just (DblValue 12)))) blankEnv

case_ConvertValueBy = assertEqual
  "crsConvertValueBy (blankWorld, undefined, theEnv) (VP AtDict (VII 1)) (VP AtDict (VII 2))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv1 =
    crsInitVariable iInput (VP AtDict (VII 1)) (VP AtHere (VIV (IntValue 12)))
  theInput1 = (blankWorld, undefined, theEnv1)
  theEnv2 =
    crsInitVariable theInput1 (VP AtDict (VII 2)) (VP AtHere (VIV (DblValue 3)))
  question = crsConvertValueBy (blankWorld, undefined, theEnv2)
                               (VP AtDict (VII 1))
                               (VP AtDict (VII 2))
  answer = setDCache
    (IM.fromList [(1, W (Just (DblValue 12))), (2, W (Just (DblValue 3)))])
    blankEnv

case_ReplaceText = assertEqual
  "crsReplaceText (blankWorld, undefined, theEnv) (VP AtHere (VIV (StrValue \"ABC${D:1}BEC\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv = crsInitVariable iInput
                           (VP AtDict (VII 1))
                           (VP AtHere (VIV (StrValue "ABC${D:1}BE${D:1}C")))
  theInput = (blankWorld, undefined, theEnv)
  question = crsReplaceText theInput (VP AtDict (VII 1))
  answer   = setDCache
    (IM.singleton
      1
      (W (Just (StrValue "ABCABC${D:1}BE${D:1}CBEABC${D:1}BE${D:1}CC")))
    )
    blankEnv

case_ReplaceTextTo = assertEqual
  "crsReplaceTextTo (blankWorld, undefined, theEnv) (VP AtHere (VIV (StrValue \"ABC${D:1}BEC\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  iInput = (blankWorld, undefined, blankEnv)
  theEnv = crsInitVariable iInput
                           (VP AtDict (VII 1))
                           (VP AtHere (VIV (StrValue "123")))
  theInput = (blankWorld, undefined, theEnv)
  question = crsReplaceTextTo
    theInput
    (VP AtHere (VIV (StrValue "ABC${D:1}BE${D:1}C")))
    (VP AtDict (VII 2))
  answer = setDCache
    (IM.fromList
      [(1, W (Just (StrValue "123"))), (2, W (Just (StrValue "ABC123BE123C")))]
    )
    blankEnv

case_SetVPosition = assertEqual
  "crsSetVPosition (blankWorld, undefined, blankEnv) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  question = crsSetVPosition (blankWorld, undefined, blankEnv)
                             (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
                             (VP AtDict (VII 1))
  answer = setDCache
    (IM.singleton
      1
      (W (Just (PtrValue (VP AtHere (VIV (StrValue "AtDict[VII=1]"))))))
    )
    blankEnv

case_GetVPosition = assertEqual
  "crsGetVPosition (blankWorld, undefined, blankEnv) (VP AtHere (VIV (StrValue \"AtDict[VII=1]\"))) (VP AtDict (VII 1))"
  answer
  question
 where
  question = crsGetVPosition (blankWorld, undefined, blankEnv)
                             (VP AtHere (VIV (StrValue "AtDict[VII=1]")))
                             (VP AtDict (VII 1))
  answer = setDCache
    (IM.singleton 1 (W (Just (PtrValue (VP AtDict (VII 1))))))
    blankEnv

case_LogAtConsole = assertEqual
  "crsLog (blankWorld, undefined, blankEnv) (VP AtTricky (VIN \"Console\"))) (VP AtHere (VIV (StrValue \"Log at Console\")))"
  answer
  question
 where
  question = crsLog (blankWorld, undefined, blankEnv)
                    (VP AtTricky (VIN "Console"))
                    (VP AtHere (VIV (StrValue "Log at Console")))
  answer =
    setTCache (HM.singleton "Console" (StrValue "Log at Console")) blankEnv

case_LogAtLogger = assertEqual
  "crsLog (blankWorld, undefined, blankEnv) (VP AtTricky (VIN \"Logger\"))) (VP AtHere (VIV (StrValue \"Log at Logger\")))"
  answer
  question
 where
  question = crsLog (blankWorld, undefined, blankEnv)
                    (VP AtTricky (VIN "Logger"))
                    (VP AtHere (VIV (StrValue "Log at Logger")))
  answer =
    setTCache (HM.singleton "Logger" (StrValue "Log at Logger")) blankEnv
