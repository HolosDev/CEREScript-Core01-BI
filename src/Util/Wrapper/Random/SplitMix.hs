module Util.Wrapper.Random.SplitMix where


import           Data.Bifunctor
import           Data.Word

import           System.Random.SplitMix        as SM


type RG = SMGen
type GSeed = (Word64, Word64)

nextWord64 :: RG -> (Word64, RG)
nextWord64 = SM.nextWord64
nextWord32 :: RG -> (Word32, RG)
nextWord32 = SM.nextWord32
nextTwoWord32 :: RG -> (Word32, Word32, RG)
nextTwoWord32 = SM.nextTwoWord32
nextInt :: RG -> (Int, RG)
nextInt = SM.nextInt
nextDouble :: RG -> (Double, RG)
nextDouble = SM.nextDouble
nextFloat :: RG -> (Float, RG)
nextFloat = SM.nextFloat

mkGen :: Word64 -> RG
mkGen = mkSMGen
mkGenFromInt :: Int -> RG
mkGenFromInt = mkSMGen . fromIntegral
initGen :: IO RG
initGen = initSMGen
newGen :: IO RG
newGen = newSMGen
seedGen :: GSeed -> RG
seedGen = uncurry seedSMGen
unseedGen :: RG -> GSeed
unseedGen = unseedSMGen
splitGen :: RG -> (RG, RG)
splitGen = splitSMGen

bmwr32 :: Word32 -> RG -> (Word32, RG)
bmwr32 = bitmaskWithRejection32
bmwr32' :: Word32 -> RG -> (Word32, RG)
bmwr32' = bitmaskWithRejection32'
bmwr64 :: Word64 -> RG -> (Word64, RG)
bmwr64 = bitmaskWithRejection64
bmwr64' :: Word64 -> RG -> (Word64, RG)
bmwr64' = bitmaskWithRejection64'
bmwrInt :: Int -> RG -> (Int, RG)
bmwrInt i = first fromIntegral . bitmaskWithRejection64 (fromIntegral i)
bmwrInt' :: Int -> RG -> (Int, RG)
bmwrInt' i = first fromIntegral . bitmaskWithRejection64 (fromIntegral i)
