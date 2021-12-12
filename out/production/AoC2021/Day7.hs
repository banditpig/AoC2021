{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day7 where

-- dist x = sum $ [(x, abs (x - x')) | x' <- [16,1,2,0,4,2,7,1,2,14], x /= x']
import Control.Arrow ((&&&))
import Data.List
import Data.List.Split
import Prelude

type Input = [Int]
fuelCost :: Int -> Int -> Int 
fuelCost x y = absxy * (absxy +1) `div` 2 where absxy = abs (x - y)

part1 :: Input -> Int
part1 xs = foldr (\x ac -> ac + abs (median - x)) 0 xs
  where
    median = (!!) (sort xs) $ length xs `div` 2

part2 :: Input -> ()
part2 = undefined

prepare s = map (\s -> read s :: Int) $ splitOn "," s

main :: IO ()
main = readFile "data/Day7.txt" >>= print . (part1 &&& part2) . prepare
