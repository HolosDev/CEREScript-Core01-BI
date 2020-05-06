{-
# In Spool module, we prepares
* Spool ID
* Spool Name
* SI Script Maker
* SI Name Maker
* SI readVP/writeVP Maker
* SI Priority Maker
* SI Marker Maker
* Initialize SI local storages
-}

module MT.Spool where


{- --# The First Initialization Spool
NOTE: The First Initialization Spool do not need Maker
NOTE: Because it can't be initiated by other SI
NOTE: and a Programmer knows the result of Maker when one writes the code
* The interpreter need only one spool setting
-}

{- --# Account Initialization Spool
# readVP
## VP AtVars (PVIN "Account-")
## 
-}

{- --# Money Transfer Spool Initiation Spool
> Get random from-Account
> Get random to-Account
> Get random password
> Get random amount of money transfer
> Set arguments for new Spool
> Initiate Spool
-}

{- --# Fixed Money Transfer Spool
> Check accounts existence
>>> If the accounts do not exist, end the Spool
> Check password
>>> If password is wrong, end the Spool
> Check money of from-Account
>>> If money is insufficient, end the Spool
> If serial numbering mode
>>> Get Serial number
>>> Increment Serial number
> If tree-forest statistics mode
>>> Increment tree-forest
> Subtract money from FROM-Account
> Add money from TO-Account
> Add money to transfer statistics
> Log transaction text
-}

{- --# Standalone Fixed Money Transfer Spool
> Check accounts existence
>>> If the accounts do not exist, end the Spool
//> Check password
//>>> If password is wrong, end the Spool
//> Check money of from-Account
//>>> If money is insufficient, end the Spool
//> If tree-forest statistics mode
//>>> Increment tree-forest
> Subtract money from FROM-Account
> Add money from TO-Account
> Log transaction text
-}

{- --# Random Money Transfer Spool
> Get random from-Account
> Get random to-Account
> Get random password
-}
