{-# LANGUAGE BinaryLiterals #-}

module Main (main) where
import Test.Hspec
import Data.Word
import Des
import MyBits


ipInput :: Word64  -- Initial permutation Input
ipInput  = 0b0000000100100011010001010110011110001001101010111100110111101111
ipOutput :: Word64 --Initial permutation Output
ipOutput = 0b1100110000000000110011001111111111110000101010101111000010101010 

lrOutput :: (Word32, Word32) -- Separate a block in 2 parts
lrOutput = (0b11001100000000001100110011111111, 0b11110000101010101111000010101010)

main :: IO () 
main = hspec $ do
  describe "test DES functions" $ do 
    it "Initial Permutation" $ do 
      Des.ip ipInput `shouldBe` ipOutput

    it "Decompose L and R" $ do 
      MyBits.getLR ipOutput `shouldBe` lrOutput
