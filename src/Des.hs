module Des 
  ( encrypt 
  , decrypt) 
  where

import Data.Word (Word64, Word32)
import Des.Internal
import Data.Bits((.^.))
import Des.Bits


--- The hidden version of encrypt just goes through the DES encryption 
--- iterations (1,2..16) recursively. The implementation is pretty simple.
--- 
encrypt' :: Word32 -> Word32 -> Word32 -> Word32 -> Int -> Word64
encrypt' l r c d iter 
  | iter == 16 = Des.Bits.joinLR r' l'
  | otherwise = encrypt' l' r' c' d' (iter + 1)
  where 
    (kn, c', d') = Des.Internal.ks c d iter
    r' = l .^. Des.Internal.f r kn
    l' = r


--- Performs the DES block encryption to a 64 bits block given a 64 bits block 
--- key. The output will be another 64 bits block.
--- 
encrypt :: Word64 -> Word64 -> Word64
encrypt block key = Des.Internal.ipInv $ encrypt' l r c d 1
  where 
    (l,r) = Des.Bits.splitLR $ Des.Internal.ip block    -- (L,R) from IP
    (c,d) = Des.Bits.splitCD $ Des.Internal.permPC1 key -- (C,D) from key


-- Decrypt is not implemented
decrypt :: Word64 -> Word64 -> Word64
decrypt _ _ = error "Not implemented"
