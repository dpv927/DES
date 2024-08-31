{-# LANGUAGE BinaryLiterals #-}

module Main (main) where
import Test.Hspec
import Data.Word
import Des
import MyBits


ipInput :: Word64  -- Initial permutation Input
ipInput = 0b0000000100100011010001010110011110001001101010111100110111101111
ipOutput :: Word64 --Initial permutation Output
ipOutput = 0b1100110000000000110011001111111111110000101010101111000010101010 


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


main :: IO () 
main = hspec $ do
  describe "test DES functions" $ do 
    it "Test Initial Permutation (IP)." $ do 
      Des.ip ipInput `shouldBe` ipOutput

    it "Test Decompose a block into 'L' and 'R'." $ do 
      MyBits.getLR ipOutput `shouldBe` lrOutput

    it "Test 'E' permutation." $ do 
      Des.e eInput `shouldBe` eOutput

    it "Test 'S' functions." $ do 
      Des.s sInput 0xfc00000000000000 1 0 `shouldBe` sOutput

    it "Test 'P' permutation." $ do 
      Des.p pInput `shouldBe` pOutput

    it "Test 'f' function." $ do 
      Des.f fInputBlock fInputKey `shouldBe` fOutput
