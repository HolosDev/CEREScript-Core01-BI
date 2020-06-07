module Util.Parallel.Granularity where

import           Debug.Trace

makeGranList :: Int -> [a] -> [[a]]
makeGranList 1 list = [list]
makeGranList n list = map reverse $ makeGranListSub (replicate n []) n list
-- makeGranList n list = trace (show $ length list) makeGranListSub (replicate n []) n list
makeGranListSub accLists _ [] = accLists
makeGranListSub accLists n list =
  let additional = take n list
      rest       = drop n list
      nAccLists  = zipSf (\x y -> y : x) id accLists additional
  in  makeGranListSub nAccLists n rest

makeGranListSimple :: Int -> [a] -> [[a]]
makeGranListSimple 1 list = [list]
makeGranListSimple n list = makeGranListSub (replicate n []) n list

zipSf :: (a -> b -> c) -> (a -> c) -> [a] -> [b] -> [c]
zipSf _ _ []       _        = []
zipSf f g (a : as) []       = g a : zipSf f g as []
zipSf f g (a : as) (b : bs) = f a b : zipSf f g as bs

reduceList :: [[a]] -> [a]
reduceList = reduceListSub []
reduceListSub accList [] = accList
reduceListSub accList lists =
  let fLists = filter (not . null) lists
      heads  = map head fLists
      tails  = map tail fLists
  in  reduceListSub (accList <> heads) tails

reduceListSimple :: [[a]] -> [a]
reduceListSimple = concat
