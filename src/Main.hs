{-# LANGUAGE BinaryLiterals #-}

module Main (main) where
import Test.Hspec
import Data.Word
import Des.Internal
import Des.Bits


ipInput :: Word64  -- Initial permutation Input
ipInput = 0b0000000100100011010001010110011110001001101010111100110111101111
ipOutput :: Word64 --Initial permutation Output
ipOutput = 0b1100110000000000110011001111111111110000101010101111000010101010 

ipInvInput :: Word64  -- Initial permutation inverse Input
ipInvInput = 0b0000101001001100110110011001010101000011010000100011001000110100
ipInvOutput :: Word64 --Initial permutation inverse Output
ipInvOutput = 0b1000010111101000000100110101010000001111000010101011010000000101

lrOutput :: (Word32, Word32) -- Separate a block in 2 parts
lrOutput = (0b11001100000000001100110011111111, 0b11110000101010101111000010101010)


eInput :: Word32  -- E function Input 
eInput = 0b11110000101010101111000010101010
eOutput :: Word64 -- E function Output
eOutput = 0b0111101000010101010101010111101000010101010101010000000000000000


sInput :: Word64  -- S functions Input 
sInput = 0b0110000100010111101110101000011001100101001001110000000000000000
sOutput :: Word32 -- S functions Output
sOutput = 0b01011100100000101011010110010111


pInput :: Word32  -- 'P' permutation input
pInput = 0b01011100100000101011010110010111
pOutput :: Word32 -- 'P' permutation output
pOutput = 0b00100011010010101010100110111011


fInputBlock :: Word32 -- f(R,K) input block (R)
fInputBlock = 0b11110000101010101111000010101010
fInputKey :: Word64   -- f(R,K) input round key (K)
fInputKey =  0b0001101100000010111011111111110001110000011100100000000000000000
fOutput :: Word32     -- f(R,K) output
fOutput = 0b00100011010010101010100110111011


pc1Input :: Word64  -- PC-1 (Permuted Choice-1 input)
pc1Input = 0b0001001100110100010101110111100110011011101111001101111111110001
pc1Output :: Word64 -- PC-1 (Permuted Choice-1 output)
pc1Output = 0b1111000011001100101010101111010101010110011001111000111100000000


cdInput :: Word64            -- Decompose C,D from key input
cdInput = 0b1111000011001100101010101111010101010110011001111000111100000000
cdOutput :: (Word32, Word32) -- Decompose C,D from key output
cdOutput = (0b11110000110011001010101011110000, 0b01010101011001100111100011110000)
c :: Word32 
d :: Word32
(c, d) = cdOutput -- For composing the block CD


pc2Input :: Word64  -- PC-2 (Permuted Choice-2 input)
pc2Input = 0b1110000110011001010101011111101010101100110011110001111000000000
pc2Output :: Word64 -- PC-2 (Permuted Choice-2 output)
pc2Output = 0b0001101100000010111011111111110001110000011100100000000000000000


ksInputC :: Word32 -- Key Schedule input C block 
ksInputC = 0b11110000110011001010101011110000
ksInputD :: Word32 -- Key Schedule input D block
ksInputD = 0b01010101011001100111100011110000
ksInputIter :: Int -- Key Schedule input Iteration
ksInputIter = 1
ksOutputKey :: Word64
ksOutputKey = 0b0001101100000010111011111111110001110000011100100000000000000000
ksOutputC :: Word32
ksOutputC = 0b11100001100110010101010111110000
ksOutputD :: Word32 
ksOutputD = 0b10101010110011001111000111100000


main :: IO () 
main = hspec $ do
  describe "test DES functions" $ do 
    it "Test Initial Permutation (IP)." $ do 
      Des.Internal.ip ipInput `shouldBe` ipOutput

    it "Test Initial Permutation Inverse (IP-1)." $ do 
      Des.Internal.ipInv ipInvInput `shouldBe` ipInvOutput

    it "Test Decompose a block into 'L' and 'R'." $ do 
      Des.Bits.getLR ipOutput `shouldBe` lrOutput

    it "Test 'E' permutation." $ do 
      Des.Internal.e eInput `shouldBe` eOutput

    it "Test 'S' functions." $ do 
      Des.Internal.s sInput 0xfc00000000000000 1 0 `shouldBe` sOutput

    it "Test 'P' permutation." $ do 
      Des.Internal.p pInput `shouldBe` pOutput

    it "Test 'f' function." $ do 
      Des.Internal.f fInputBlock fInputKey `shouldBe` fOutput

    it "Test PC-1 (Permuted Choice-1)." $ do
      Des.Internal.permPC1 pc1Input `shouldBe` pc1Output

    it "Test Decompose key block into 'C' and 'D'." $ do 
      Des.Bits.splitCD cdInput `shouldBe` cdOutput

    it "Test Compose key block from 'C' and 'D'." $ do 
      Des.Bits.joinCD c d `shouldBe` cdInput

    it "Test PC-2 (Permuted Choice-2)." $ do 
      Des.Internal.permPC2 pc2Input `shouldBe` pc2Output

    it "Test Key Shedule (KS) function." $ do 
      Des.Internal.ks ksInputC ksInputD ksInputIter `shouldBe` (ksOutputKey, ksOutputC, ksOutputD)
