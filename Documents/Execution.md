Execution
====

## Primary Issues

### State/Storage

* World State/Dictをどう持ち越すか？
* それぞれのSpoolの変更結果をどう反映するか？
* それぞれのSpool InstanceのLocal Stateをどう持ち越すか？

### Spool and Spool Instance

* Spool Instanceをどう認識・区別するか？
* 実行途中であるSpoolのポインターの保存方法
* 実行途中のSpoolを次のTime-slotに持ち越す方法

## How to work?

### When after executing a time-slot

* Returns
  * World
    * ValueList' - Do I need this with current `Value` definition?
    * WorldState'
    * SpoolInstances'

### How SpoolInstance remains after running

* Do not add Control Code as a independent type in `Value`
  * `Value` would handle the Control Code as a independent type in other version
* Reserved

### How to (add/remove) readVP/writeVP from SpoolInstance

Use LocalStorage

### Running nest structure

Serialized Execution

* `Input` :: `(World,SI,Env)`
* `Env` :: `(WorldCache, LocalCache, TrickCache, RG)`

* runSimulator :: `Time -> World -> World`
  * runTimeSlot :: `World -> World`
    * siAggregator :: `World -> SpoolForest`
    * runSpoolTree :: `SpoolTree -> World -> (SpoolTree, WorldCache)`
      * cacheMaker :: `SpoolTree -> World -> WorldCache`
      * runSpoolInstance :: `World -> SI -> WorldCache  -> ((SIParams,SI),Env)`
        * runCEREScript :: `Input -> CEREScript -> (Env, CEREScript)`
          * runInstruction :: `Input -> CERES -> Env`
      * cacheCommitter' :: `WorldState -> [WorldCache] -> WorldState`


## How to avoid Variable ID collision

### Allocate pre-divided Variable ID range for each SpoolTree

#### Issues

* Q: How to check already allocated Variable?
  * A: Interpreter gives `World` to every interpreting function.
* When a region is fulfilled, what can I do?
  * A: Currently, we do nothing. But we can know that situation by using sub-IntMap's size.

#### Protocol

* Use left-side in range finding function in `containers-hack`
* Follow avoiding hash collision protocol
  * Get a new random number
  * If a new ID is already used, then return the first
  * Else, use it


