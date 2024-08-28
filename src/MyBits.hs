module MyBits 
  ( setBit
  , testBit
  , getLR )
  where

import Data.Word (Word64, Word32)
import Data.Bits ((.&.), (.|.), (.>>.))


-- Mask for a block's first bit
firstBit :: Word64 -- 0b1000....0
firstBit = 0x8000000000000000


-- Set the 'i'th bit of the block to 1
setBit :: Word64 -> Int -> Word64 
setBit block i = block .|. (firstBit .>>. i)


-- Is the 'i'th bit of a block != 0?
testBit :: Word64 -> Int -> Bool 
testBit block i = (block .&. (firstBit .>>. i)) /= 0 


-- Get the highest 32-bits of a block (L), and 
-- the lowest 32-bits (R) in a pair (L, R).
getLR :: Word64 -> (Word32, Word32)
getLR block = (l, r)
  where
   l = fromIntegral ((0xffffffff00000000 .&. block) .>>. 32) :: Word32
   r = fromIntegral (0x00000000ffffffff .&. block) :: Word32
