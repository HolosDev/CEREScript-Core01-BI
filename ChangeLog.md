Changelog for CEREScript-Core01-BI
====

## Unreleased changes

### Add
* Implement each `crs*` instruction function

### Change
* Change design for backpack compatibility
* Study how to handle readVP/writeVP in CERESSpool
* Use ValueContainer instead of Value
* Implement parallel `updateValuesToVT`
* Use `getValues*` instead of using `foldr` in `cacheMaker`
* Implement better `Eq` instance for handling `AtWorld` and `AtTime` simultaneously


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
