module MyBits 
  ( setBit64
  , testBit64
  , setBit32
  , testBit32
  , getLR )
  where

import Data.Word (Word64, Word32)
import Data.Bits ((.&.), (.|.), (.>>.))


-- Mask for a 64-bits block's first bit
firstBit64 :: Word64 -- 0b1000....0
firstBit64 = 0x8000000000000000


-- Set the 'i'th bit of a 64-bits block to 1
setBit64 :: Word64 -> Int -> Word64 
setBit64 block i = block .|. (firstBit64 .>>. i)


-- Is the 'i'th bit of a 64-bits block != 0?
testBit64 :: Word64 -> Int -> Bool 
testBit64 block i = (block .&. (firstBit64 .>>. i)) /= 0 


-- Mask for a 32-bits block's first bit
firstBit32 :: Word32 -- 0b1000....0
firstBit32 = 0x80000000


-- Set the 'i'th bit of a 32-bits block to 1
setBit32 :: Word32 -> Int -> Word32
setBit32 block i = block .|. (firstBit32 .>>. i)


-- Is the 'i'th bit of a 32-bits block != 0?
testBit32 :: Word32 -> Int -> Bool 
testBit32 block i = (block .&. (firstBit32 .>>. i)) /= 0 


-- Get the highest 32-bits of a block (L), and 
-- the lowest 32-bits (R) in a pair (L, R).
getLR :: Word64 -> (Word32, Word32)
getLR block = (l, r)
  where
   l = fromIntegral ((0xffffffff00000000 .&. block) .>>. 32) :: Word32
   r = fromIntegral (0x00000000ffffffff .&. block) :: Word32
