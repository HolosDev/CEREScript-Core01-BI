module Util where


fst3 (a, _, _) = a
snd3 (_, b, _) = b
trd3 (_, _, c) = c

-- NOTE: Almostly same as double `fromMaybe`
recover :: Maybe v -> Maybe v -> Maybe v
recover Nothing whenFail = whenFail
recover success _        = success
