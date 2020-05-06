module MT.Script where


import Data.CERES.Data
import Data.CERES.Operator

import MT.Data


vpHere a = VP AtHere (VIV a)
vpHereI = vpHere . IntValue
vpHereD = vpHere . DblValue
vpHereB = vpHere . BoolValue
vpHereS = vpHere . StrValue
vpHereP = vpHere . PtrValue
vpHereA = vpHere . ArrValue
vpHereAtom = vpHere AtomValue

vpPtr a = VP AtPtr (VIV (PtrValue a))

theFirstInitializationScript = [ CRSNoop
  , CRSLog (VP AtTricky (VIN "Logger")) (vpHereS "Start the first initialization")
  -- //> Initialize log variable: Needs LstValue

  -- > Initialize mode variable
  -- >> initialize transfer serial numbering variable
  , CRSInitVariable (VP AtVars (VIN "ModeSerialNum")) (vpHereB False)
  -- >> initialize transfer amount counting variable
  , CRSInitVariable (VP AtVars (VIN "ModeTxCnt")) (vpHereB False)
  -- >> initialize account usage counting variable
  , CRSInitVariable (VP AtVars (VIN "ModeAccCnt")) (vpHereB False)
  -- >> initialize password check counting variable
  , CRSInitVariable (VP AtVars (VIN "ModePassCheckCnt")) (vpHereB False)
  -- >> initialize tree-forest counting variable
  , CRSInitVariable (VP AtVars (VIN "ModeTFCnt")) (vpHereB False)

  -- > Initialize transfer serial number variable
  , CRSInitVariable (VP AtVars (VIN "SerialNum")) (vpHereI 0)
  -- > Initialize transaction counter variable
  , CRSInitVariable (VP AtVars (VIN "TxCnt")) (vpHereI 0)
  -- > Initialize account counter variable
  , CRSInitVariable (VP AtVars (VIN "AccCnt")) (vpHereI 0)

  -- //> Initialize statistics variable
  -- //>> Initialize tree-forest statistics variable
  -- //>> Initialize transfer statistics variable
  -- //>> Initialize password check statistics variable

  -- % Initiate Random Account Generator SI
  -- %% Set arguments of the SI
  -- %%% Set Account size
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVAccountSize")) (vpHereI 16)
  -- %%% Set Account size
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVAccountGenerationNum")) (vpHereI 1)
  -- %% Initiate the SI
  , CRSInitVariable (VP AtLNTemp (VIN "TargetVariable-Name")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "TargetVariable-SIID")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "LoopAcc")) (vpHereI 0)
  ]
  -- %% Loop for 15 instance
  ++ (concat . replicate 15 $ randomAccountGeneratorSILauncherScript)
  ++ [ CRSNoop
  -- %% Delete arguments
  -- TODO: Not yet implemented, Replace with a new instruction
  , CRSToInterpreter0 "DeletePassingArguments"
  , CRSDeleteVariable (VP AtLNVars (VIN "ArgsNVAccountSize"))
  , CRSDeleteVariable (VP AtLNVars (VIN "ArgsNVAccountGenerationNum"))

  -- % Initiate Random Transfer Generator SI
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVTransferGenerationNum")) (vpHereI 64)
  -- %% Set arguments of the SI
  -- %% Initiate the SI
  , CRSSIInit (vpHereI randomTransferGeneratorSpoolID) (vpHereS "RandomTransferGenerator") (VP AtNDict (VIN "SIID-RandomTransferGenerator")) (vpHereI 0)
  -- %% Delete arguments
  -- TODO: Not yet implemented, Replace with a new instruction
  , CRSToInterpreter0 "DeletePassingArguments"
  , CRSDeleteVariable (VP AtLNVars (VIN "ArgsNVTransferGenerationNum"))

  , CRSInitVariable (VP AtLTemp (VII (-1))) (vpHereS "Abolish")

  , CRSLog (VP AtTricky (VIN "Console")) (vpHereS "End the first initialization")
  , CRSToInterpreter0 "Debug:LVars"
  , CRSToInterpreter0 "Debug:LNVars"
  , CRSToInterpreter0 "Debug:LTemp"
  , CRSToInterpreter0 "Debug:LNTemp"
  ]

randomAccountGeneratorSILauncherScript =
  [ CRSNoop
  , CRSModifyValue2 (VP AtLNTemp (VIN "LoopAcc")) (vpHereI 1) COAAdd
  , CRSReplaceTextTo (vpHereS "Name-RandomAccountGenerator${LNT:LoopAcc}") (VP AtLNTemp (VIN "TargetVariable-Name"))
  -- ? AtLNTemp[VIN="TargetVariable-Name"] = StrValue "AtLNTemp[VIN=\"Name-RandomAccountGenerator??\"]"
  , CRSGetVPosition (VP AtLNTemp (VIN "TargetVariable-Name")) (VP AtLNTemp (VIN "TargetVariable-Name"))
  -- ? AtLNTemp[VIN="TargetVariable-Name"] = PtrValue "AtLNTemp[VIN="Name-RandomAccountGenerator??"]"
  , CRSInitVariable (vpPtr (VP AtLNTemp (VIN "TargetVariable-Name"))) vpHereAtom
  -- ? AtLNTemp[VIN="Name-RandomAccountGenerator??"] = AtomValue
  , CRSReplaceTextTo (vpHereS "RandomAccountGenerator-${LNT:LoopAcc}") (vpPtr (VP AtLNTemp (VIN "TargetVariable-Name")))
  -- ? AtLNTemp[VIN="Name-RandomAccountGenerator??"] = StrValue "RandomAccountGenerator-??"

  , CRSReplaceTextTo (vpHereS "SIID-RandomAccountGenerator${LNT:LoopAcc}") (VP AtLNTemp (VIN "TargetVariable-SIID"))
  -- ? AtLNTemp[VIN="TargetVariable-SIID"] = StrValue "AtLNTemp[VIN=\"SIID-RandomAccountGenerator??\"]"
  , CRSGetVPosition (VP AtLNTemp (VIN "TargetVariable-SIID")) (VP AtLNTemp (VIN "TargetVariable-SIID"))
  -- ? AtLNTemp[VIN="TargetVariable-SIID"] = PtrValue "AtLNTemp[VIN="SIID-RandomAccountGenerator??\"]"
  , CRSInitVariable (vpPtr (VP AtLNTemp (VIN "TargetVariable-SIID"))) vpHereAtom
  -- ? AtLNTemp[VIN="SIID-RandomAccountGenerator??"] = AtomValue

  , CRSSIInit (vpHereI randomAccountGeneratorSpoolID) (vpPtr (VP AtLNTemp (VIN "TargetVariable-Name"))) (vpPtr (VP AtLNTemp (VIN "TargetVariable-SIID"))) (vpHereI 0)
  ]

accountGeneratorScript =
  [ CRSNoop
  , CRSLog (VP AtTricky (VIN "Logger")) (vpHereS "")
  , CRSInitVariable (VP AtLNTemp (VIN "AccountID")) (vpHereB False)
  -- > Get ID of a new account ID
  -- >> Get Random Int within [0,AccountSize)
  , CRSRandomWith (VP AtLNTemp (VIN "AccountID")) VTInt (vpHereS "Uniform") (vpHereI 0) (VP AtLNVars (VIN "AccountSize"))
  -- >> Get NKey of a new account
  , CRSConvertValue (VP AtLNTemp (VIN "AccountID")) VTStr
  -- ? AtLNTemp[VIN="AccountID"] = StrValue "??"
  -- TODO: Replace this with VI modifier/generator
  , CRSReplaceTextTo (vpHereS "Account-${AtLNTemp[VIN=\"AccountID\"]}") (VP AtLNTemp (VIN "AccountID"))
  -- ? AtLNTemp[VIN="AccountID"] = StrValue "Account-??"
  -- > Check the new account NKey is exist or not
  , CRSInitVariable (VP AtLNTemp (VIN "VExist")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "VPToCheck")) vpHereAtom
  -- >> Generate VP string of the new account
  -- >> Get VP from StrValue
  , CRSReplaceTextTo (vpHereS "AtNTime[VIN=\"${LNT:AccountID}\"]") (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = StrValue "AtNTime[VIN="Account-??"]"
  , CRSGetVPosition (VP AtLNTemp (VIN "VPToCheck")) (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = PtrValue AtNTime[VIN="Account-??"]
  -- >> Check the existence
  , CRSCheckVariable (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "VExist"))
  -- >> When the NKey does exist, end the spool
  , CRSSIEnd (VP AtLNTemp (VIN "VExist"))

  -- //> Get random password

  -- > Set random amount of balance
  -- >> Get Random Int within [1000,2000)
  , CRSRandomWith (VP AtLNTemp (VIN "AccountBalance")) VTInt (vpHereS "Uniform") (vpHereI 1000) (vpHereI 2000)
  -- >> Initialize the account
  , CRSInitVariable (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "AccountBalance"))
  -- ? AtNTime[VIN="Account-??"] = IntValue ??

  , CRSInitVariable (VP AtLTemp (VII (-1))) (vpHereS "Abolish")

  , CRSLog (VP AtTricky (VIN "Console")) (vpHereS "End account generation")
  , CRSToInterpreter0 "Debug:LVars"
  , CRSToInterpreter0 "Debug:LNVars"
  , CRSToInterpreter0 "Debug:LTemp"
  , CRSToInterpreter0 "Debug:LNTemp"
  ]

transferGeneratorScript =
  [ CRSLog (VP AtTricky (VIN "Log")) (vpHereS "")
  , CRSInitVariable (VP AtVars (VIN "TxCnt")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVTxCnt")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVFromAccount")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVToAccount")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVFromAccountID")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVToAccountID")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "VExist")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "VPToCheck")) vpHereAtom
  , CRSInitVariable (VP AtLNVars (VIN "ArgsNVTransferAmount")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "MoneyTransferSIID")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "MoneyTransferSIName")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "JumpTo")) (vpHereI 0)
  ]
  ++ (concat . replicate 64 $ transferGeneratorLoop1Script) ++
  [ CRSNoop

  , CRSInitVariable (VP AtLTemp (VII (-1))) (vpHereS "Abolish")

  , CRSLog (VP AtTricky (VIN "Console")) (vpHereS "End transfer generation")
  , CRSToInterpreter0 "Debug:LVars"
  , CRSToInterpreter0 "Debug:LNVars"
  , CRSToInterpreter0 "Debug:LTemp"
  , CRSToInterpreter0 "Debug:LNTemp"
  ]
-- CRSLog (VP AtTricky (VIN "Log")) (vpHereS "")

transferGeneratorLoop1Script = CRSModifyValue2 (VP AtLNTemp (VIN "JumpTo")) (vpHereI 1) COAAdd : (concat . replicate 16 $ transferGeneratorCoreScript)

transferGeneratorCoreScript =
  [ CRSNoop
  -- > Get TxCnt for new SI
  , CRSCopyValue (VP AtNVars (VIN "TxCnt")) (VP AtLNVars (VIN "ArgsNVTxCnt"))
  -- > Get ID of a new FROM account ID
  -- >> Get Random Int within [0,64)
  , CRSRandomWith (VP AtLNTemp (VIN "ArgsNVFromAccountID")) VTInt (vpHereS "Uniform") (vpHereI 0) (vpHereI 64)
  -- ? AtLNTemp[VIN="ArgsNVFromAccountID"] = IntValue ??
  -- >> Get NKey of a new account
  , CRSConvertValue (VP AtLNTemp (VIN "ArgsNVFromAccountID")) VTStr
  -- ? AtLNTemp[VIN="ArgsNVFromAccountID"] = StrValue "??"
  , CRSReplaceTextTo (vpHereS "Account-${LNV:ArgsNVFromAccountID}") (VP AtLNTemp (VIN "ArgsNVFromAccountID"))
  -- ? AtLNTemp[VIN="ArgsNVFromAccountID"] = Str "Account-??"
  -- > Check the new account NKey is exist or not
  -- >> Generate VP string of the new account
  , CRSReplaceTextTo (vpHereS "AtNTime[VIN=\"${LNV:ArgsNVFromAccountID}\"]") (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = StrValue "AtNTime[VIN="Account-??"]"
  , CRSGetVPosition (VP AtLNTemp (VIN "VPToCheck")) (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = PtrValue AtNTime[VIN="Account-??"]
  , CRSCheckVariable (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "VExist"))
  , CRSModifyValue1 (VP AtLNTemp (VIN "VExist")) (COE1 "Not")
  , CRSSIEnd (VP AtLNTemp (VIN "VExist"))
  , CRSCopyValue (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "ArgsNVFromAccount"))
  -- ? AtLNTemp[VIN="ArgsNVFromAccount"] = PtrValue AtNTime[VIN="Account-??"]

  -- > Get ID of a new TO account ID
  -- >> Get Random Int within [0,64)
  , CRSRandomWith (VP AtLNTemp (VIN "ArgsNVToAccountID")) VTInt (vpHereS "Uniform") (vpHereI 0) (vpHereI 64)
  -- ? AtLNTemp[VIN="ArgsNVToAccountID"] = IntValue ??
  -- >> Get NKey of a new account
  , CRSConvertValue (VP AtLNTemp (VIN "ArgsNVToAccountID")) VTStr
  -- ? AtLNTemp[VIN="ArgsNVToAccountID"] = StrValue "??"
  , CRSReplaceTextTo (vpHereS "Account-${LNV:ArgsNVToAccountID}") (VP AtLNTemp (VIN "ArgsNVToAccountID"))
  -- ? AtLNTemp[VIN="ArgsNVToAccountID"] = Str "Account-??"
  -- > Check the new account NKey is exist or not
  -- >> Generate VP string of the new account
  , CRSReplaceTextTo (vpHereS "AtNTime[VIN=\"${LNV:ArgsNVToAccountID}\"]") (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = StrValue "AtNTime[VIN="Account-??"]"
  , CRSGetVPosition (VP AtLNTemp (VIN "VPToCheck")) (VP AtLNTemp (VIN "VPToCheck"))
  -- ? AtLNTemp[VIN="VPToCheck"] = PtrValue AtNTime[VIN="Account-??"]
  , CRSCheckVariable (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "VExist"))
  , CRSModifyValue1 (VP AtLNTemp (VIN "VExist")) (COE1 "Not")
  , CRSSIEnd (VP AtLNTemp (VIN "VExist"))
  , CRSCopyValue (vpPtr (VP AtLNTemp (VIN "VPToCheck"))) (VP AtLNTemp (VIN "ArgsNVToAccount"))
  -- ? AtLNTemp[VIN="ArgsNVToAccount"] = PtrValue AtNTime[VIN="Account-??"]

  {-
  //> Get random password
  //>> Get Random Int within [0,4)
  , CRSRandomWith (VP AtLNTemp (VIN "ArgsNVPassword")) VTInt (vpHereS "Uniform") (vpHereI 0) (vpHereI 4)
  -}

  -- > Get random amount of money transfer
  -- >> Init variable
  -- >> Get Random Int within [100,200)
  , CRSRandomWith (VP AtLNTemp (VIN "ArgsNVTransferAmount")) VTInt (vpHereS "Uniform") (vpHereI 100) (vpHereI 200)
  -- ? AtLNTemp[VIN="ArgsNVTransferAmount"] = IntValue ??

  -- > Initiate Spool
  -- >> Get Transfer Counter
  , CRSModifyValue2 (VP AtVars (VIN "TxCnt")) (vpHereI 1) COAAdd
  -- >> Increment Transfer Count after getting value
  , CRSCopyValue (VP AtLNTemp (VIN "MoneyTransferSIID")) (VP AtVars (VIN "TxCnt"))
  , CRSReplaceTextTo (vpHereS "Money Transfer #${LNT:MoneyTransferSIID} ${LNV:ArgsNVFromAccountID} => ${LNV:ArgsNVToAccountID}") (VP AtLNTemp (VIN "MoneyTransferSIName"))
  -- >> Initiate Spool
  , CRSSIInit (vpHereI moneyTransferSpoolID) (VP AtLNTemp (VIN "MoneyTransferSIName")) (VP AtNull VINull) (VP AtLNTemp (VIN "JumpTo"))
  ]

transferScript = [ CRSNoop
  -- ? (VP AtLNVars (VIN "TxCnt"))          = IntValue ??
  -- ? (VP AtLNVars (VIN "FromAccountID"))  = StrValue "Account-??"
  -- ? (VP AtLNVars (VIN "ToAccountID"))    = StrValue "Account-??"
  -- ? (VP AtLNVars (VIN "FromAccount"))    = PtrValue AtNTime[VIN="Account-??"]
  -- ? (VP AtLNVars (VIN "ToAccount"))      = PtrValue AtNTime[VIN="Account-??"]
  -- ? (VP AtLNVars (VIN "TransferAmount")) = IntValue ??
  , CRSInitVariable (VP AtLNTemp (VIN "VExist")) vpHereAtom
  -- > Check accounts existence
  -- >>> If the accounts do not exist, end the Spool
  , CRSCheckVariable (vpPtr (VP AtLNVars (VIN "FromAccount"))) (VP AtLNTemp (VIN "VExist"))
  , CRSModifyValue1 (VP AtLNTemp (VIN "VExist")) (COE1 "Not")
  , CRSSIEnd (VP AtLNTemp (VIN "VExist"))
  , CRSCheckVariable (vpPtr (VP AtLNVars (VIN "ToAccount"))) (VP AtLNTemp (VIN "VExist"))
  , CRSModifyValue1 (VP AtLNTemp (VIN "VExist")) (COE1 "Not")
  , CRSSIEnd (VP AtLNTemp (VIN "VExist"))
  -- //> Check password
  -- //>>> If password is wrong, end the Spool
  -- //> Check money of from-Account
  -- //>>> If money is insufficient, end the Spool
  -- //> If serial numbering mode
  -- //>>> Get Serial number
  -- //>>> Increment Serial number
  -- //> If tree-forest statistics mode
  -- //>>> Increment tree-forest
  -- > Subtract money from FROM-Account
  , CRSModifyValue2 (vpPtr (VP AtLNVars (VIN "FromAccount"))) (VP AtLNVars (VIN "TransferAmount")) COASub
  -- > Add money from TO-Account
  , CRSModifyValue2 (vpPtr (VP AtLNVars (VIN "ToAccount"))) (VP AtLNVars (VIN "TransferAmount")) COAAdd
  -- //> Add money to transfer statistics
  -- > Log transaction text
  , CRSInitVariable (VP AtLNTemp (VIN "LogWhere")) vpHereAtom
  , CRSReplaceTextTo (vpHereS "AtNTime[VIN=\"${LNV:FromAccountID}=>${LNV:ToAccountID}\"]") (VP AtLNTemp (VIN "LogWhere"))
  , CRSGetVPosition (VP AtLNTemp (VIN "LogWhere")) (VP AtLNTemp (VIN "LogWhere"))
  , CRSInitVariable (VP AtLNTemp (VIN "LogContent")) vpHereAtom
  , CRSInitVariable (VP AtLNTemp (VIN "AmountMoneyStr")) (VP AtLNVars (VIN "TransferAmount"))
  , CRSConvertValue (VP AtLNTemp (VIN "AmountMoneyStr")) VTStr
  , CRSReplaceTextTo (vpHereS "${LNV:FromAccountID}=${LNV:AmountMoneyStr}=>${LNV:ToAccountID}") (VP AtLNTemp (VIN "LogContent"))
  , CRSSetValue (vpPtr (VP AtLNTemp (VIN "LogWhere"))) (vpHereS "Send")

  , CRSInitVariable (VP AtLTemp (VII (-1))) (vpHereS "Abolish")

  , CRSLog (VP AtTricky (VIN "Console")) (vpHereS "End transfer")
  , CRSToInterpreter0 "Debug:LVars"
  , CRSToInterpreter0 "Debug:LNVars"
  , CRSToInterpreter0 "Debug:LTemp"
  , CRSToInterpreter0 "Debug:LNTemp"
  ]
