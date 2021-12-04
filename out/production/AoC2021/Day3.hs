{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day3 where

import Data.Text (Text)
import InputParsers
import Text.Parsec
import Text.Parsec.String
import Prelude
import Data.Char (digitToInt)
import Data.List (foldl') 

data BinStr = BinStr String deriving (Show)

binStr :: Parser BinStr
binStr = do
  bits <- many1 ((char '1' <|> char '0') <|> (char '0' <|> char '1')) <* newline
  pure (BinStr bits)

allBinStr :: Parser [BinStr]
allBinStr = many binStr

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

countOnes :: Num b => (a, b) -> (Char, b) -> (Char, b)
countOnes (v, w) (x, y) = if x == '1' then ('1', w + 1) else ('1', w)

flipBits :: String -> String
flipBits   = fmap (\b -> if b =='1' then '0' else '1') 

part1 :: [BinStr] -> Int
part1 bs = gamma * epsilon
  where
    pairs = fmap (\(BinStr s) -> zip s (repeat 1)) bs
    columnCounts = foldr (\l acc -> zipWith countOnes acc l) (zip (repeat '1') (repeat 0)) pairs
    bits = fmap (\(_, n) -> if n > half then '1' else '0') columnCounts
      where
        half = (length bs) `div` 2
    gamma = toDec bits
    epsilon = toDec (flipBits bits)

-- ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]   
day3 :: IO ()
day3 = do
  lns <- withData "data/Day3.txt" allBinStr
  print (part1 lns)
