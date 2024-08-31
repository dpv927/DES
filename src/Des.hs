{-# LANGUAGE BinaryLiterals #-}

module Des 
  ( ip
  , ipInv 
  , e 
  , s 
  , p
  , f )
  where

import MyBits(testBit64, setBit64, testBit32, setBit32)
import Data.Bits((.^.), (.&.), (.>>.), (.<<.), (.|.))
import Data.Word(Word64, Word32, Word8)
import Data.Array


-- The functions 'permFromList64' and 'permFromList32' are used to do permutation of bits
-- inside a block (64 or 32 bits wide). To do so, a table/list of indexes is passed and
-- each bit of the block is set to the value of the corresponding bit index (from the 
-- same block).
-- 
-- Lets say that we have the following block: 1010, same as bits[1,0,1,0] (first index = 1)
-- The following list: [2,3]
-- The output would be: 0100 = [bits[2],bits[3],0,0]
--
-- The parameters are the following:
--  - Table/List of permutation indexes.
--  - Index of the bit to change.
--  - A block (accumulator of all permutations).
--  - The initial input block (constant).
--
permFromList64 :: [Int] -> Int -> Word64 -> Word64 -> Word64
permFromList64 [] _ block _ = block
permFromList64 (x:xs) destBit block initBlock = permFromList64 xs (destBit+1) newBlock initBlock
  where 
    newBlock = if testBit64 initBlock x then 
      setBit64 block destBit
    else block

permFromList32 :: [Int] -> Int -> Word32 -> Word32 -> Word32
permFromList32 [] _ block _ = block
permFromList32 (x:xs) destBit block initBlock = permFromList32 xs (destBit+1) newBlock initBlock
  where 
    newBlock = if testBit32 initBlock x then 
      setBit32 block destBit
    else block


ipIndexes :: [Int]
ipIndexes = -- IP bit indexes order
  [ 57, 49, 41, 33, 25, 17, 09, 01,
    59, 51, 43, 35, 27, 19, 11, 03,
    61, 53, 45, 37, 29, 21, 13, 05,
    63, 55, 47, 39, 31, 23, 15, 07,
    56, 48, 40, 32, 24, 16, 08, 00,
    58, 50, 42, 34, 26, 18, 10, 02,
    60, 52, 44, 36, 28, 20, 12, 04,
    62, 54, 46, 38, 30, 22, 14, 06 ]


-- Initial Permutation (IP)
-- Takes a 64 bit block and does the permutation of bits given by the
-- permutation table 'ipIndexes'. The result is another 64 bit block.
--
-- See page 10 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
ip :: Word64 -> Word64
ip = permFromList64 ipIndexes 0 0 


ipInvIndexes :: [Int]
ipInvIndexes = -- IP^-1 bit indexes
  [ 39, 07, 47, 15, 55, 23, 63, 31,
    38, 06, 46, 14, 54, 22, 62, 30,
    37, 05, 45, 13, 53, 21, 61, 29,
    36, 04, 44, 12, 52, 20, 60, 28,
    35, 03, 43, 11, 51, 19, 59, 27,
    34, 02, 42, 10, 50, 18, 58, 26,
    33, 01, 41, 09, 49, 17, 57, 25,
    32, 00, 40, 08, 48, 16, 56, 24 ]


-- Initial Permutation Inverse (IP^-1)
-- Takes a 64 bit block and does the permutation of bits given by the
-- permutation table 'ipInvIndexes', which is the inverse of 'ipIndexes'.
-- The result is another 64 bit block.
--
-- See page 10 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
ipInv :: Word64 -> Word64
ipInv = permFromList64 ipInvIndexes 0 0


eIndexes :: [Int]
eIndexes = -- E bit selection
  [ 31, 00, 01, 02, 03, 04, 
    03, 04, 05, 06, 07, 08,
    07, 08, 09, 10, 11, 12,
    11, 12, 13, 14, 15, 16,
    15, 16, 17, 18, 19, 20,
    19, 20, 21, 22, 23, 24,
    23, 24, 25, 26, 27, 28,
    27, 28, 29, 30, 31, 00 ]


-- Takes a 32 bit block and yields a block of 48 bits (as a 64 bit padded block).
-- To do so, applies the permutation given by the table 'eIndexes'.
--
-- See page 13 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
e :: Word32 -> Word64 
e block = permFromList64 eIndexes 0 0 paddedBlock
  where paddedBlock = (fromIntegral block :: Word64) .<<. 32


sRow :: [Int]
sRow = [0,1..15]

sTables :: Array Int (Array Int (Array Int Word8))
sTables = array (1,8) [
  (1, -- S1
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7]),
      (1, array (0,15) $ zip sRow [0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8]),
      (2, array (0,15) $ zip sRow [4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0]),
      (3, array (0,15) $ zip sRow [15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13])]),
  (2, -- S2
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10]),
      (1, array (0,15) $ zip sRow [3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5]),
      (2, array (0,15) $ zip sRow [0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15]),
      (3, array (0,15) $ zip sRow [13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9])]),
  (3, -- S3
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8]),
      (1, array (0,15) $ zip sRow [13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1]),
      (2, array (0,15) $ zip sRow [13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7]),
      (3, array (0,15) $ zip sRow [1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12])]),
  (4, -- S4
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15]),
      (1, array (0,15) $ zip sRow [13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9]),
      (2, array (0,15) $ zip sRow [10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4]),
      (3, array (0,15) $ zip sRow [3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14])]),
  (5, -- S5
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9]),
      (1, array (0,15) $ zip sRow [14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6]),
      (2, array (0,15) $ zip sRow [4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14]),
      (3, array (0,15) $ zip sRow [11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3])]),
  (6,  -- S6
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11]),
      (1, array (0,15) $ zip sRow [10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8]),
      (2, array (0,15) $ zip sRow [9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6]),
      (3, array (0,15) $ zip sRow [4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13])]),
  (7, -- S7 
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1]),
      (1, array (0,15) $ zip sRow [13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6]),
      (2, array (0,15) $ zip sRow [1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2]),
      (3, array (0,15) $ zip sRow [6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12])]),
  (8, -- S8
    array (0,3) [ 
      (0, array (0,15) $ zip sRow [13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7]),
      (1, array (0,15) $ zip sRow [1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2]),
      (2, array (0,15) $ zip sRow [7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8]),
      (3, array (0,15) $ zip sRow [2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11])])]


-- Calculates the S(it, i, j) functions to each group of 6 bits from a 
-- 48-bit wide block (result from the 'e' function). Returns 
-- a 32-bit wide block with the result of each Si function.
--
-- See page 14 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
-- 
-- Params:
--  - Initial input block (48-bit block padded as a 64-bit block). Will be constant.
--  - Bit mask. Masks 6 bits of the Initial input block. The resultant 6 bits will help to obtain S(it, i, j) 
--  - Iteration number. Describes which S function we have to apply.
--  - Resultant block. 
--
s :: Word64 -> Word64 -> Int -> Word32 -> Word32
s _ _ 9 block = block
s initBlock mask iter block = s initBlock (mask .>>. 6) (iter+1) (block .|. sres)
  where 
    -- Apply mask to the block: get 6 bits
    b = (mask .&. initBlock) .>>. (64 - (6 * iter))
    row = (0b000001 .&. b) .|. ((0b100000 .&. b) .>>. 4) 
    col = (0b011110 .&. b) .>>. 1
    sij = ((sTables ! iter) ! (fromIntegral row :: Int)) ! (fromIntegral col :: Int)
    sres = (fromIntegral sij :: Word32) .<<. (32 - (4 * iter))


-- Permutation 'P'
-- indexes.
pIndexes :: [Int]
pIndexes = [
  15, 06, 19, 20,
  28, 11, 27, 16,
  00, 14, 22, 25,
  04, 17, 30, 09,
  01, 07, 23, 13,
  31, 26, 02, 08,
  18, 12, 29, 05,
  21, 10, 03, 24 ]


-- Takes a 32 bit block and yields another permuted block of 32 bits.
-- To do so, applies the permutation given by the table 'pIndexes'.
--
-- See page 15 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
p :: Word32 -> Word32
p = permFromList32 pIndexes 0 0


-- The 'f' function is described as the most complex function of the DES algorithm.
-- The procedure of this function is the following:
--
--  - Takes a 32 bits block (R) and a round key (K). 
--  - Applies the permutation 'E' to the block, obtaining a brand-new 48 bits block.
--  - The result is XORed with the 48 bits wide round key (K).
--  - The result of the last operation is passed to the 's' function, obtaining a 32 bit block.
--  - Finally, we apply the permutation 'P' to the last block.
--
--  As always in this implementation, blocks of 48 bits are stored padded in a 64 bit block.
--  See page 13 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
f :: Word32 -> Word64 -> Word32
f block key = p $ s (e block .^. key) 0xfc00000000000000 1 0
