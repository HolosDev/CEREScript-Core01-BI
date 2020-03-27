Internal Time
====

Internal Time means that smaller time unit than a time-slot.

## Definition

`worldTSSize` Internal time is same as 1 Time-Slot.

## When Interpreting

### Summary

Only `TimeElapse` instruction spend internal time.

### Before start a SpoolInstance

Check `resume` flag.
When `resume` flag is set, then add `elapsedTime` and existing `executingTime` and mod with `worldTSSize`.

Otherwise, get a new `executingTime` for starting base regardless any `sp` flag.
And set the value to `elapsedTime`.

### Interpret a CERES

Run any instruction time freely.

When executing `TimeElapse` instruction, 
* Get a new `executingTime`
* Modify `elapsedTime` with `executingTime`

When a instruction is `Stop` or `Pause`, set `elapsedTime` as 0.



