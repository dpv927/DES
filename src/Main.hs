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


sInput :: Word64 -- S functions Input 
sInput = 0b0110000100010111101110101000011001100101001001110000000000000000
sOutput :: Word32 -- S functions Output
sOutput = 0b01011100100000101011010110010111


main :: IO () 
main = hspec $ do
  describe "test DES functions" $ do 
    it "Initial Permutation" $ do 
      Des.ip ipInput `shouldBe` ipOutput

    it "Decompose L and R" $ do 
      MyBits.getLR ipOutput `shouldBe` lrOutput

    it "E permutation function" $ do 
      Des.e eInput `shouldBe` eOutput

    it "S functions" $ do 
      Des.s sInput 0xfc00000000000000 1 0 `shouldBe` sOutput
