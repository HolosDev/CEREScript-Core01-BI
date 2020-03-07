Structure
====

## Data Storage

### World

Global state with time.

`HistoricTable` takes every EpochRow.

### Dict

A global state without time.

### (AtTime)

Not independent storage.
Based on WorldState.

### Local

Storage which belongs to a spool instance which is lasting after a time slot.

### Cache

Storage which belongs to a spool instance which is not lasting after a time slot.
Stores temporal/focused variable only.

### (AtHere)

No storage.
Stored in an instruction itself.

## Instruction

### AtWorld

A global state with absolute time definition.
Without time notation, the time is set to 0.

### AtDict

A global state without time.

### AtTime

 A global state with relative time definition.
Without time notation, the time is set to `current`.

### AtLocal

Local state which limited in a spool instance.

### AtCache

Local state which limited in a spool instance in a time-slot.

### AtHere

Just for using it, not for storing.


## Value retention in a Table

The `Value` or `ValueContainer` does not require retention description to keep it.
Without explicit `DeleteVariable` instruction, a new Time-Slot would copy every value in the last Time-Slot.

## Reserved Storage

### WorldState

* 0: Simulation Control
  * "Run": run next Time-Slot
  * "Stop": stop the simulation
* 1: 

### LocalState

* 0: Retention code of `SpoolInstance`
  * "Retain": keep the `SpoolInstance`
  * "Init": keep the `SpoolInstance`, but do not keep `LocalState`
  * "Abolish": end the `SpoolInstance`
* 1: Jump the retained `SpoolInstance`
* 16~23: Add readVP/writeVP in SpoolInstance
  * Usually, BI does not add a new VP when Spool initiate Variable
* 24~31: Remove readVP/writeVP in SpoolInstance
  * Usually, BI do not remove a VP when Spool initiate Variable
* 32~47: Invoke a new `SpoolInstance`
  * Should remove number limitation by an array
* 48~63: Stop other `SpoolInstance`
  * Should remove number limitation by an array

