module Util where


import           Data.IntMap                   as IM
import           Data.Trie.Text                as Trie


fst3 (a, _, _) = a
snd3 (_, b, _) = b
trd3 (_, _, c) = c

recover :: v -> Maybe v -> Maybe v
recover whenFail Nothing = Just whenFail
recover whenFail success = success

vMapUpdate idx mValue = IM.update (const mValue) idx
nvMapUpdate nKey mValue vnMap =
  maybe (Trie.delete nKey vnMap) (\v -> Trie.insert nKey v vnMap) mValue
