------------------------------------------------------------------------------
-- | 
-- Module         : Data.ReusableIdentifiers
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : BSD-style (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Data.ReusableIdentifiers
    ( Record, newRecord, newId, freeId, recordLimit )
    where

import Prelude hiding (replicate, map, length)
import Control.Monad (msum)
import Data.Bits
import Data.Word
import Data.Int
import Data.Vector.Storable hiding ((++))
import System.Random

data Record = Record Int StdGen !(Vector Int)

unit = finiteBitSize (undefined :: Int)

-- | New record with (at least) the given size.
newRecord :: Int -> Record
newRecord s = Record (n * unit) (mkStdGen s) vec
   where
        (a, b) = s `divMod` unit
        n      = a + min 1 b
        vec    = replicate n zeroBits

newId :: Record -> Maybe (Int, Record)
newId (Record s gen vec) = msum [ f i j | i <- [piv .. length vec - 1] ++ [0 .. piv], j <- [0 .. unit - 1] ]
    where
        (n, gen')                     = next gen
        piv                           = n `mod` length vec
        f i j | (vec ! i) `testBit` j = Nothing
              | otherwise             = Just (i * unit + j, Record s gen' $ vec `unsafeUpd` [(i, (vec ! i) `setBit` j)])

freeId :: Int -> Record -> Record
freeId x (Record s gen vec) = Record s gen $ unsafeUpd vec [(fromIntegral i, (vec ! fromIntegral i) `clearBit` fromIntegral j)]
    where (i,j) = x `divMod` unit

-- | Get the exact upper limit on the identifiers.
recordLimit :: Record -> Int
recordLimit (Record s _ _) = s
