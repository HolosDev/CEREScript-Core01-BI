module CERES.BI.Util.PQueue where


import           Data.Maybe
import           Data.IntMinMaxQueue           as IPQ


empty = IPQ.empty

singleton = IPQ.singleton

null = IPQ.null

insert = IPQ.insert

peekMin = IPQ.peekMin

peekMin' = fromJust . IPQ.peekMin

deleteMin = IPQ.deleteMin

pollMin = IPQ.pollMin

pollMin' = fromJust . IPQ.pollMin

takeMin = IPQ.takeMin

dropMin = IPQ.dropMin

fromList = IPQ.fromList

fromListWith = IPQ.fromListWith
