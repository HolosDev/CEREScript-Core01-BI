Storage
====

## Category of Storage

### World, NWorld

Global storage with time.

`HistoricalTable` takes every EpochRow.
`NHistoricalTable` takes every NEpochRow.

### Dict, NDict

Global storage which are not dependent on time.
The entries on the Dict/NDict should not be changed its value frequently.

### Vars, NVars

Global storage which are not dependent on time.
The entries on the Vars/NVars would be changed its value frequently.

### LocalVars, LocalNVars

Local storage which belong to a spool instance which is lasting after a time slot.

### LocalTemp, LocalNTemp

Local storage which belongs to a spool instance which is not lasting after a time slot.
Stores temporal/focused variable only.

### Register

Local storage which belongs to an ST monad which is not lasting exterior of the ST monad.
Any Register would be initialized with the initial value at first.

### The actual initial value

* `AtomValue`
  * Pros
    * Efficient than `ErrValue *`
  * Cons
    * Does not provide useful message/log for programmer when the script access/modify an uninitialized value
* `ErrValue "[ERROR]<Register:=:Init> Not Initialized"`

### (Here)

Stateless storage.
Stored in an instruction/script itself.

### (Null)

Stateless storage.
When you don't want to give any target VP to storing instruction.

## VariablePlace

### AtWorld, AtNWorld

Global storage with absolute time index.
Without time notation, the time is set to 0.

#### Compatible `VariableIndex`
* `VIIT` it directs definite VP on `HistoricalTable`
  * `VII i` is assumed as `VIIT i 0`
* `VINT` it directs definite VP on `NHistoricalTable`
  * `VIN n` is assumed as `VIN n 0`
* `VIpNT pn t` it directs any matching VPs which one's prefix is `pn` on `NHistoricalTable`
  * `VIpN pn` is assumed as `VINT pn 0`

### AtTime, AtNTime

Global storage with relative time index.
Without time notation, the time is set to `current`.

* `VIIT i t` it directs definite VP on `HistoricalTable`
  * `VII i` is assumed as `VIIT i current`
* `VINT n t` it directs definite VP on `NHistoricalTable`
  * `VIN n` is assumed as `VINT n current`
* `VIpNT pn t` it directs any matching VPs which one's prefix is `pN` on `NHistoricalTable`
  * `VIpN pn` is assumed as `VINT pn current`

### AtDict, AtNDict, AtVars, AtNVars

Global storage which are not dependent on time.

#### Compatible `VariableIndex`

* `VII i` it directs VP on `Dict` or `Vars`
* `VIN n` it directs VP on `NDict` or `NVars`
* `VIpN pn` it directs any matching VPs which one's prefix is `pN` on `NDict` or `NVars`

### AtLVars, AtLNVars

Local storage which limited in a spool instance.

#### Compatible `VariableIndex`

* `VII i` it directs VP on `LVars`
* `VIN n` it directs VP on `LNVars`
* `VIpN pn` it directs any matching VPs which one's prefix is `pN` on `LNVars`

### AtLTemp, AtLNTemp

Local storage which limited in a spool instance in a time-slot.

#### Compatible `VariableIndex`

* `VII i` it directs VP on `LTemp`
* `VIN n` it directs VP on `LNTemp`
* `VIpN pn` it directs any matching VPs which one's prefix is `pN` on `LNTemp`

### AtReg

Local storage which belongs to an ST monad which is not lasting exterior of the ST monad.

#### Compatible `VariableIndex`

* `VII i` it directs a index of pre-allocated Registers

### AtHere

Just for using it instantly, not for storing.

#### Compatible `VariableIndex`

* `VIV v` only

### AtNull

Do nothing with the VP.

#### Compatible `VariableIndex`

* `VINull` only

### AtAtom

Place holder, resemble with `AtNull`, but is not `AtNull`.

### AtPtr

Pointer type. It could take a value from anywhere.

#### Compatible `VariableIndex`

* `VIPtr` only

### AtTricky

This not actually directs a storage.

#### Compatible `VariableIndex`

* Usually, `VIN`, but could take anything

## Issues

### `P*` VariableIndex series

This is not used for VariablePosition in usual.
This is for R/WVP checking.

### Value retention in a `HistoricalTable`

The `Value` or `ValueContainer` does not require a retention description to keep it.
Without explicit `DeleteVariable` instruction, a new Time-Slot would copy every value in the last Time-Slot.
