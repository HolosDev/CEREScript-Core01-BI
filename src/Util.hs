module Util where


fst3 (a, _, _) = a
snd3 (_, b, _) = b
trd3 (_, _, c) = c

recover :: v -> Maybe v -> Maybe v
recover whenFail Nothing = Just whenFail
recover whenFail success = success
