Changelog for CEREScript-Core01-BI
====

## Unreleased changes

### Add
* Implement `runSpoolInstance`
* Implement `runCEREScript`
* Implement `runInstruction`

### Change
* Study how to handle readVP/writeVP in CERESSpool
* Use ValueContainer instead of Value
* Implement parallel `updateValuesToVT`
* Use `getValues*` instead of using `foldr` in `cacheMaker`
* Implement better `Eq` instance for handling `AtWorld` and `AtTime` simultaneously


## 0.2.0.0 -- 2020-03-08

### Added
* Implement `cacheCommitter`
* Implement `siisExecutor`
* Implement `updateWorldState`
* Filter and Unwrap RW from WorldCache and apply to WorldState

### Changed
* Move SIIS definition to Data


## 0.1.0.0 -- 2020-03-06

### Added
* Inherit codes from CEREScript-Core01
* Implement basic tree-forest aggregation algorithm
* Implement basic cacheMaker

### Changed
* Change some data and type names from original
