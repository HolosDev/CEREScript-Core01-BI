module CERES.BI.Type
  ( module CERES.BI.Type
  , CERES.BI.Util.Random.RG
  , CERES.BI.Util.Random.GSeed
  )
where


import           Data.Maybe

-- NOTE: Alias for abstract PRNG type
import           CERES.BI.Util.Random


data Maker s f a = Maker { mDef :: a, mF :: f, mMaker :: s -> f -> Maybe a }

runMaker Maker {..} s = fromMaybe mDef (mMaker s mF)

type InternalTime = Int
