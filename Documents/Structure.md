Structure
====

## Data Storage

### World, NWorld

Global state with time.

`HistoricalTable` takes every EpochRow.
`NHistoricalTable` takes every NEpochRow.

### Dict, NDict

A global state which are not dependent on time.
The entries on the Dict/NDict should not change its value frequently.

### Vars, NVars

A global state which are not dependent on time.
The entries on the Vars/NVars would change its value frequently.

### (AtTime, AtNTime)

Not independent storage.
Based on `HistoricalTable` and `NHistoricalTable`.

### LocalVars, LocalNVars

Storage which belongs to a spool instance which is lasting after a time slot.

### LocalTemp, LocalNTemp

Storage which belongs to a spool instance which is not lasting after a time slot.
Stores temporal/focused variable only.

### (AtHere)

No storage.
Stored in an instruction itself.

### (AtNull)

When you don't want to give any target VP to storing instruction.

## VariablePosition

### AtWorld, AtNWorld

A global state with absolute time definition.
Without time notation, the time is set to 0.

### AtDict, AtNDict, AtVars, AtNVars

A global state which are not dependent on time.

### AtTime, AtNTime

 A global state with relative time definition.
Without time notation, the time is set to `current`.

### AtLVars, AtLNVars

Local state which limited in a spool instance.

### AtLTemp, AtLNTemp

Local state which limited in a spool instance in a time-slot.

### AtHere

Just for using it, not for storing.

### AtNull

Do nothing with the VP


## Value retention in a Table

The `Value` or `ValueContainer` does not require retention description to keep it.
Without explicit `DeleteVariable` instruction, a new Time-Slot would copy every value in the last Time-Slot.

## Reserved Storage

### WorldVars

* 0: Simulation Control
  * "Run": run next Time-Slot
  * "Stop": stop the simulation
* 1: 

### LocalVariables

* -1: Reserved for Control Flow
* -2: Reserved for Control Flow
* -3: ExecutingTime for a next CERES instruction
  * Need to be initialized before add in SITable
* -4: Elapsed InternalTime in time-slot
* -5: Resume or not
  * True: do not get a new executingTime
  * otherwise: get a new executingTime

### LocalNVariables

* "ArgXXX": Given Arguments to an SI
* "PassArgXXX": For passing Arguments to another initiating SI
* "CallArgXXX": For passing Arguments to calling SI
* "RtrnRsltXXX": Returning Value to caller SI
* "RtrnValueXXX": Returned value from calling SI

### LocalTemp

Stores only variables which should not be inherited after time-slot


* 0~15: Reserved for passing arguments
* -1: Retention code of `SpoolInstance`
  * "Retain": keep the `SpoolInstance`
  * "Forget": keep the `SpoolInstance`, but do not keep `LocalVariables`
  * "Init": Initiate `CEREScript` and `LocalVariables`
  * "Abolish": end the `SpoolInstance`
* -2: Jump the retained `SpoolInstance`
* -4: Stop or Pause immediately
  * "Stop": stop and remove rest CEREScript
  * "Pause": pause in this time-slot
  * otherwise: continue
* -16~23: Add readVP/writeVP in SpoolInstance
  * Usually, BI does not add a new VP when Spool initiate Variable
* -24~31: Remove readVP/writeVP in SpoolInstance
  * Usually, BI do not remove a VP when Spool initiate Variable
* -32~47: Invoke a new `SpoolInstance`
  * Should remove number limitation by an array
* -48~63: Stop other `SpoolInstance`
  * Should remove number limitation by an array

### LocalNTemp
