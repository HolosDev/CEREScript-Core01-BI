module MT.Data where


import Data.CERES.Type


-- % Configuration config
-- %% Mode configuration

transactionCountingMode :: NKey
transactionCountingMode = "ModeTxCnt"

-- %% Counter configuration

-- | counter of account
accountCounter :: NKey
accountCounter = "AccCnt"

-- | counter of transaction
transactionCounter :: NKey
transactionCounter = "TxCnt"

-- | Spool ID
randomAccountGeneratorSpoolID = 1 :: ID
randomTransferGeneratorSpoolID = 2 :: ID
moneyTransferSpoolID = 3 :: ID
