Changelog for CEREScript-Core01-BI
====

## Unreleased changes

### Add
* Implement each `crs*` instruction function
* Add `Util.Random` module for random function aliases


### Change
* Change design for backpack compatibility
* Study how to handle readVP/writeVP in CERESSpool
* Use ValueContainer instead of Value
* Implement parallel `updateValuesToVT`
* Use `getValues*` instead of using `foldr` in `cacheMaker`
* Implement better `Eq` instance for handling `AtWorld` and `AtTime` simultaneously
* Move Move `get*Cache` and `set*Cache` functions to `CERES.BI.Data.*`


## 0.5.0.0 -- 2020-03-26

### Changed
* Update dependency of CEREScript-Core 0.10.0.0


## 0.4.0.0 -- 2020-03-12

### Added
* Implement new `set*` and `get*` for Cache
* Define RG type and move to `CERES.BI.Type`

### Changed
* Update dependency of CEREScript-Core 0.9.0.0
* Change old function name `get*` and `set*` to `get*By` and `set*By`


## 0.3.1.0 -- 2020-03-08

### Fixed
* Fill non-exhaustive patters
* Fix wrong localVariables is given in `runSpoolInstance`

### Removed
* Remove unused definition


## 0.3.0.0 -- 2020-03-08

### Added
* Implement `runSpoolInstance`
* Implement `runCEREScript`
* Implement `runInstruction`

### Changed
* Add PRNG generator field to `WorldState` and `SpoolInstance`


## 0.2.0.0 -- 2020-03-08

### Added
* Implement `updateWorld`
* Implement `cacheCommitter`
* Implement `siisExecutor`
* Implement `updateWorldState`
* Filter and Unwrap RW from WorldCache and apply to WorldState
* Add type `LocalCache`
* Add InternalTime fields to `World`
* Add initialLocalVariables and initialLocalCache field to `CERESSpool`
* Add constants for interpreter control variables' ID
* Add more fields to `SpoolInstance`
  * LocalState
  * Original Spool ID
  * Rest CEREScript

### Changed
* Move SIIS definition to Data
* Redesign/Rename `LocalState`, `LocalVariables` and `LocalCache`

### Fixed
* Add missing `siisExecutorSub`


## 0.1.0.0 -- 2020-03-06

### Added
* Inherit codes from CEREScript-Core01
* Implement basic tree-forest aggregation algorithm
* Implement basic cacheMaker

### Changed
* Change some data and type names from original
