{-# LANGUAGE BinaryLiterals #-}

module Des 
  ( ip
  , ipInv 
  , e 
  , s 
  , p
  , f
  , permPC1
  , permPC2
  , splitCD
  , joinCD 
  , ks )
  where

import MyBits(testBit64, setBit64, testBit32, setBit32)
import Data.Bits((.^.), (.&.), (.>>.), (.<<.), (.|.))
import Data.Word(Word64, Word32, Word8)
import Data.Array


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


-- Permuted choice 
-- index table 1
pc1Indexes :: [Int]
pc1Indexes = [
  56, 48, 40, 32, 24, 16, 08,
  00, 57, 49, 41, 33, 25, 17,
  09, 01, 58, 50, 42, 34, 26,
  18, 10, 02, 59, 51, 43, 35,
  62, 54, 46, 38, 30, 22, 14,
  06, 61, 53, 45, 37, 29, 21,
  13, 05, 60, 52, 44, 36, 28,
  20, 12, 04, 27, 19, 11, 03 ]


-- Permuted choice 
-- index table 2
pc2Indexes :: [Int]
pc2Indexes = [
   13, 16, 10, 23, 00, 04,
   02, 27, 14, 05, 20, 09,
   22, 18, 11, 03, 25, 07,
   15, 06, 26, 19, 12, 01,
   40, 51, 30, 36, 46, 54,
   29, 39, 50, 44, 32, 47,
   43, 48, 38, 55, 33, 52,
   45, 41, 49, 35, 28, 31 ]


cdShifts :: Array Int Int -- Iteration shifts of C & D from Key Schedule
cdShifts = array (1,16) $ zip [1..16] [1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]


-- The functions 'permFromList64' and 'permFromList32' are used to do 
-- permutation of bits inside a block (64 or 32 bits wide). To do so, a 
-- table/list of indexes is passed and each bit of the block is set to the 
-- value of the corresponding bit index (from the same block).
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


--- ``Initial Permutation (IP)``
--- Takes a 64 bit block and does the permutation of bits given by the
--- permutation table 'ipIndexes'. The result is another 64 bit block.
---
ip :: Word64 -> Word64
ip = permFromList64 ipIndexes 0 0 


--- ``Initial Permutation Inverse (IP^-1)``
--- Takes a 64 bit block and does the permutation of bits given by the
--- permutation table 'ipInvIndexes'. The result is another 64 bit block.
---
ipInv :: Word64 -> Word64
ipInv = permFromList64 ipInvIndexes 0 0


--- ``E function``
--- Takes a 32 bit block and yields a block of 48 bits (as a 64 bit padded block).
--- To do so, applies the permutation given by the table 'eIndexes'. Is part of
--- the 'f' function.
--
e :: Word32 -> Word64 
e block = permFromList64 eIndexes 0 0 paddedBlock
  where paddedBlock = (fromIntegral block :: Word64) .<<. 32


--- Calculates the S(it, i, j) functions to each group of 6 bits from a 
--- 48-bit wide block (result from the 'e' function). Returns 
--- a 32-bit wide block with the result of each `S`iter function.
---
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


--- ``P function``
--- The permutation function P yields a 32-bit output from a 32-bit input by
--- permuting the bits of the input block. Such a function is defined by the 
--- list/table `pIndexes`.
---
p :: Word32 -> Word32
p = permFromList32 pIndexes 0 0


--- ``F function``
--- The 'f' function is described as the most complex function of the DES 
--- algorithm. Is equivalent to f(R,K) = P(S1(E(R) + K)..S8(E(R) + K)))
---
f :: Word32 -> Word64 -> Word32
f block key = p $ s (e block .^. key) 0xfc00000000000000 1 0


--- Applies the PC-1 (Permuted Choice-1) permutation to a 64 bit block. This 
--- block will be a key and the output will be a 56 bit block padded as a 64 
--- bit block.
---
permPC1 :: Word64 -> Word64
permPC1 = permFromList64 pc1Indexes 0 0


--- Applies the PC-2 (Permuted Choice-2) permutation to a 58 bit block. This 
--- block will be a CnDn block and the output will be a 48 bit block padded as 
--- a 64 bit block (which will be a Key Kn).
---
permPC2 :: Word64 -> Word64
permPC2 = permFromList64 pc2Indexes 0 0


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
rotateBlockL :: Word32 -> Int -> Word32
rotateBlockL block shifts 
 | shifts == 2 = (block .<<. shifts) .|. firstTwoBits
 | shifts == 1 = (block .<<. shifts) .|. firstBit
 | otherwise = 0
 where 
  firstBit = (block .&. 0x80000000) .>>. 27
  firstTwoBits = (block .&. 0xc0000000) .>>. 26


--- ``Key Schedule function (KS)``
--- Generates a Key (Kn) from two 28 bits blocks Cn-1 and Dn-1 and a iteration 
--- number (1,2,..16) from the DES encryption. Apart from the generated key, new 
--- modified blocks Cn and Dn are returned.
---
ks :: Word32 -> Word32 -> Int -> (Word64, Word32, Word32)
ks c d iter = (permPC2 $ joinCD cn dn, cn, dn)
  where 
    shifts = cdShifts ! iter
    cn = rotateBlockL c shifts
    dn = rotateBlockL d shifts
