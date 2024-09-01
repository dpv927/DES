module Des.Bits 
  ( setBit64
  , testBit64
  , setBit32
  , testBit32
  , getLR
  , splitCD
  , joinCD 
  , rotateBlock28Left )
  where

import Data.Word (Word64, Word32)
import Data.Bits ((.&.), (.|.), (.>>.), (.<<.))


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


--- Splits a 58 bit block into two 28 bit blocks (each one padded as a 32 
--- bit block). This will be the input of the Key Schedule function (KS).
---
splitCD :: Word64 -> (Word32, Word32)
splitCD block = (c, d)
  where 
    c = fromIntegral ((block .&. 0xFFFFFFF000000000) .>>. 32) :: Word32
    d = fromIntegral ((block .&. 0xFFFFFFF00) .>>. 4 ) :: Word32


--- Joins two 28 bit blocks (padded as 32 bit blocks) into one 58 bit block
--- (padded as a 64 bit block). This will be the input of the Permuted Choice-2.
---
joinCD :: Word32 -> Word32 -> Word64
joinCD c d = cPad .|. dPad
  where 
    cPad = (fromIntegral c :: Word64) .<<. 32
    dPad = (fromIntegral d :: Word64) .<<. 4


--- Rotates the bits of a 28 bit word to the left. The overflow bits are 
--- 'pasted' at the the last 1 or 2 bits of the 28 bits.
---
rotateBlock28Left :: Word32 -> Int -> Word32
rotateBlock28Left block shifts 
 | shifts == 2 = (block .<<. shifts) .|. firstTwoBits
 | shifts == 1 = (block .<<. shifts) .|. firstBit
 | otherwise = 0
 where 
  firstBit = (block .&. 0x80000000) .>>. 27
  firstTwoBits = (block .&. 0xc0000000) .>>. 26
