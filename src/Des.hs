{-# LANGUAGE BinaryLiterals #-}

module Des 
  ( ip
  , ipInv 
  , e 
  , s 
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
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [14, 04, 13, 01, 02, 15, 11, 08, 03, 10, 06, 12, 05, 09, 00, 07]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [00, 15, 07, 04, 14, 02, 13, 01, 10, 06, 12, 11, 09, 05, 03, 08]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [04, 01, 14, 08, 13, 06, 02, 11, 15, 12, 09, 07, 03, 10, 05, 00]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [15, 12, 08, 02, 04, 09, 01, 07, 05, 11, 03, 14, 10, 00, 06, 13]])]),
  (2, -- S2
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [15, 01, 08, 14, 06, 11, 03, 04, 09, 07, 02, 13, 12, 00, 05, 10]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [03, 13, 04, 07, 15, 02, 08, 14, 12, 00, 01, 10, 06, 09, 11, 05]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [00, 14, 07, 11, 10, 04, 13, 01, 05, 08, 12, 06, 09, 03, 02, 15]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [13, 08, 10, 01, 03, 15, 04, 02, 11, 06, 07, 12, 00, 05, 14, 09]])]),
  (3, -- S3
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [10, 00, 09, 14, 06, 03, 15, 05, 01, 13, 12, 07, 11, 04, 02, 08]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [13, 07, 00, 09, 03, 04, 06, 10, 02, 08, 05, 14, 12, 11, 15, 01]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [13, 06, 04, 09, 08, 15, 03, 00, 11, 01, 02, 12, 05, 10, 14, 07]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [01, 10, 13, 00, 06, 09, 08, 07, 04, 15, 14, 03, 11, 05, 02, 12]])]),
  (4, -- S4
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [07, 13, 14, 03, 00, 06, 09, 10, 01, 02, 08, 05, 11, 12, 04, 15]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [13, 08, 11, 05, 06, 15, 00, 03, 04, 07, 02, 12, 01, 10, 14, 09]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [10, 06, 09, 00, 12, 11, 07, 13, 15, 01, 03, 14, 05, 02, 08, 04]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [03, 15, 00, 06, 10, 01, 13, 08, 09, 04, 05, 11, 12, 07, 02, 14]])]),
  (5, -- S5
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [02, 12, 04, 01, 07, 10, 11, 06, 08, 05, 03, 15, 13, 00, 14, 09]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [14, 11, 02, 12, 04, 07, 13, 01, 05, 00, 15, 10, 03, 09, 08, 06]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [04, 02, 01, 11, 10, 13, 07, 08, 15, 09, 12, 05, 06, 03, 00, 14]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [11, 08, 12, 07, 01, 14, 02, 13, 06, 15, 00, 09, 10, 04, 05, 03]])]),
  (6,  -- S6
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [12, 01, 10, 15, 09, 02, 06, 08, 00, 13, 03, 04, 14, 07, 05, 11]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [10, 15, 04, 02, 07, 12, 09, 05, 06, 01, 13, 14, 00, 11, 03, 08]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [09, 14, 15, 05, 02, 08, 12, 03, 07, 00, 04, 10, 01, 13, 11, 06]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [04, 03, 02, 12, 09, 05, 15, 10, 11, 14, 01, 07, 06, 00, 08, 13]])]),
  (7, -- S7 
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [04, 11, 02, 14, 15, 00, 08, 13, 03, 12, 09, 07, 05, 10, 06, 01]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [13, 00, 11, 07, 04, 09, 01, 10, 14, 03, 05, 12, 02, 15, 08, 06]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [01, 04, 11, 13, 12, 03, 07, 14, 10, 15, 06, 08, 00, 05, 09, 02]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [06, 11, 13, 08, 01, 04, 10, 07, 09, 05, 00, 15, 14, 02, 03, 12]])]),
  (8, -- S8
    array (0,2) [ 
      (0, array (0,15) [(i,j) | i <- sRow, j <- [13, 02, 08, 04, 06, 15, 11, 01, 10, 09, 03, 14, 05, 00, 12, 07]]),
      (1, array (0,15) [(i,j) | i <- sRow, j <- [01, 15, 13, 08, 10, 03, 07, 04, 12, 05, 06, 11, 00, 14, 09, 02]]),
      (2, array (0,15) [(i,j) | i <- sRow, j <- [07, 11, 04, 01, 09, 12, 14, 02, 00, 06, 10, 13, 15, 03, 05, 08]]),
      (3, array (0,15) [(i,j) | i <- sRow, j <- [02, 01, 14, 07, 04, 10, 08, 13, 15, 12, 09, 00, 03, 05, 06, 11]])])]


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
s initBlock mask it block = s initBlock (mask .>>. 6) (it+1) (block .|. sres)
  where 
    -- Apply mask to the block: get 6 bits
    b = (mask .&. initBlock) .>>. (48 - (6 * it))
    row = (0b000001 .&. b) .|. ((0b100000 .&. b) .>>. 5) 
    col = (0b011110 .&. b) .>>. 1
    sij = ((sTables ! it) ! (fromIntegral row :: Int)) ! (fromIntegral col :: Int)
    sres = (fromIntegral sij :: Word32) .<<. (32 - (4 * it))


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
--  - The result of the last separation is passed to the 's' function, obtaining a 32 bits block.
--  - Finally, we apply the permutation 'P' to the last block.
--
--  See page 13 of https://csrc.nist.gov/files/pubs/fips/46-3/final/docs/fips46-3.pdf
--
f :: Word32 -> Word64 -> Word32
f block key = p  $ s postXor 0xfc0000000000 1 0
  where
    -- E(R) XOR RoundKey -> Get only 48 bits
    postXor = (e block .^. key) .&. 0xffffffffffff
