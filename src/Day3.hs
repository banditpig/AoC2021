{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day3 where

import Control.Arrow ((&&&))
import Data.List (foldl', transpose)
import Prelude

data Bit = Zero | One deriving (Show, Enum, Eq)
data Freq = Freq {ones :: Int, zeroes :: Int} deriving (Show)
type Input = [[Bit]]

instance Semigroup Freq where
  Freq o z <> Freq o' z' = Freq (o + o') (z + z')

instance Monoid Freq where
  mempty = Freq 0 0

toDec :: [Bit] -> Int
toDec = foldl' (\acc x -> acc * 2 + fromEnum x) 0

flipBits :: Bit -> Bit
flipBits One = Zero
flipBits _ = One

update :: Bit -> Freq -> Freq
update One (Freq o z) = Freq (o + 1) z
update Zero (Freq o z) = Freq o (z + 1)

deriveBits :: [Freq] -> [Bit]
deriveBits = foldl' (\ac (Freq o z) -> if o >= z then ac <> [One] else ac <> [Zero]) []

countBits :: Foldable t => t Bit -> Freq
countBits = foldMap asFreq -- this works because Freq is a Monoid
  where
    asFreq One = Freq 1 0
    asFreq Zero = Freq 0 1

part1 :: Input -> Int
part1 allBits = result
  where
    bitsCount = map countBits (transpose allBits)
    -- Apply gamma and epsilon over deriveBits bitsCount and multiply their values.
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    -- Think about it  like this
    -- (*) <$> sum <*> product $  [1,2,3,4]
    -- apply product to [1,2,3,4]
    -- apply sum to [1,2,3,4]
    -- then to multiply fmap (*) over the two results
    result = (*) <$> gamma <*> epsilon $ deriveBits bitsCount
      where
        gamma   = toDec
        epsilon = toDec . map flipBits

calcFreqs :: [[Bit]] -> [Freq]
calcFreqs allBits = map countBits $ transpose allBits


calcScrubOrGen whichBit  allBits  = go allBits (calcFreqs allBits) 0 where
  go (b:[]) _ _ = toDec b 
  go bbs  freqs  ix  = go filtrdList (calcFreqs filtrdList) (ix + 1) where 
    filtrdList = (filterList bbs ix (whichBit ( (!!) freqs ix)) )
   

part2:: Input -> Int
part2 allBits = scrubber * generator where
  whichBitS (Freq ones  zeroes) = if zeroes <= ones then Zero else One
  whichBitG (Freq ones  zeroes) = if zeroes <= ones then One else Zero
         
  scrubber  = calcScrubOrGen whichBitS allBits
  generator = calcScrubOrGen whichBitG allBits

filterList :: [[Bit]] -> Int -> Bit -> [[Bit]]
filterList allBits ix bt = filter (\bits -> (!!) bits ix == bt) allBits

prepare :: String -> Input
prepare = map (map toBit) . lines
  where
    toBit '1' = One
    toBit '0' = Zero

day3 :: IO ()
day3 = readFile "data/Day3.txt" >>= print . (part1 &&& part2) . prepare
