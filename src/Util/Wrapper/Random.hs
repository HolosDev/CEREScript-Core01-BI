{- Util.Wrapper.Random description

Wrapper series's module does not have its own body (function or alias).
Wrapper user should
1. Define one's own Wrapper module in their package
2. Select and Import Wrapper's sub module in the Wrapper module and re-export it

-}

module Util.Wrapper.Random where


import           Data.Word


type RG = Int
type GSeed = Int

nextWord64 :: RG -> (Word64, RG)
nextWord64 = error "Import from sub module"
nextWord32 :: RG -> (Word32, RG)
nextWord32 = error "Import from sub module"
nextTwoWord32 :: RG -> (Word32, Word32, RG)
nextTwoWord32 = error "Import from sub module"
nextInt :: RG -> (Int, RG)
nextInt = error "Import from sub module"
nextDouble :: RG -> (Double, RG)
nextDouble = error "Import from sub module"
nextFloat :: RG -> (Float, RG)
nextFloat = error "Import from sub module"

mkGen :: Word64 -> RG
mkGen = error "Import from sub module"
mkGenFromInt :: Int -> RG
mkGenFromInt = error "Import from sub module"
initGen :: IO RG
initGen = error "Import from sub module"
newGen :: IO RG
newGen = error "Import from sub module"
seedGen :: GSeed -> RG
seedGen = error "Import from sub module"
unseedGen :: RG -> GSeed
unseedGen = error "Import from sub module"
splitGen :: RG -> (RG, RG)
splitGen = error "Import from sub module"

bmwr32 :: Word32 -> RG -> (Word32, RG)
bmwr32 = error "Import from sub module"
bmwr32' :: Word32 -> RG -> (Word32, RG)
bmwr32' = error "Import from sub module"
bmwr64 :: Word64 -> RG -> (Word64, RG)
bmwr64 = error "Import from sub module"
bmwr64' :: Word64 -> RG -> (Word64, RG)
bmwr64' = error "Import from sub module"
bmwrInt :: Int -> RG -> (Int, RG)
bmwrInt = error "Import from sub module"
bmwrInt' :: Int -> RG -> (Int, RG)
bmwrInt' = error "Import from sub module"
