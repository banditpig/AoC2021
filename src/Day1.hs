{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day1 where

import InputParsers
import Prelude

offsetPairs :: Int -> (a -> a -> b) -> [a] -> [b]
offsetPairs n combine xs = zipWith (flip combine) xs (drop n xs)

part1 :: (Num a, Ord a) => [a] -> Int
part1  = length . filter (> 0) . offsetPairs 1 (-) 

part2 :: (Num a, Ord a) => [a] -> Int
part2  = length . filter (> 0) . offsetPairs 3 (-) 

day1 :: IO ()
day1 = do
  withData "data/Day1.txt" parserListInt
    >>= ( \x -> do
            print . part1 $ x
            print . part2 $ x
        )

day1' :: IO ()
day1' = do
  nums <- withData "data/Day1.txt" parserListInt
  print $ part1 nums
  print $ part2 nums

day1'' :: IO ()
day1'' = do
  withData "data/Day1.txt" parserListInt >>= print . part1
  withData "data/Day1.txt" parserListInt >>= print . part2
