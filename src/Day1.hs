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

diffs :: (Num a) => [a] -> [a]
diffs xs@(_ : xxs) = zipWith (-) xxs xs

window :: (Num a) => [a] -> [a]
window (a : b : c : xxs) = (a + b + c) : window (b : c : xxs)
window _ = []

part1 :: (Num a, Ord a) => [a] -> Int
part1 = length . filter (> 0) . diffs

part2 :: (Num a, Ord a) => [a] -> Int
part2 = length . filter (> 0) . diffs . window

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
