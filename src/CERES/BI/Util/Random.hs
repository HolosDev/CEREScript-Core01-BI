{-# LANGUAGE TypeSynonymInstances #-}

module CERES.BI.Util.Random
  ( module Random
  )
where


import           Util.Adaptor.Random.SplitMix  as Random

instance Eq RG where
  a == b = True -- NOTE: This definition is for test only
  -- a == b = unseedGen a == unseedGen b
