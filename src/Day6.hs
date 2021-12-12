{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day6 where

import Control.Arrow ((&&&))
import Data.List.Split
import qualified Data.Map as M
import Prelude

stepMp :: M.Map Integer Integer -> M.Map Integer Integer
stepMp s = M.unionWith (+) changeAges nextGen
  where
    changeAges = M.fromList $ do
      prev <- [1 .. 8]
      pure (prev - 1, get prev)
    nextGen =
      let nm = get 0
       in M.fromList [(6, nm), (8, nm)]
    get k = M.findWithDefault 0 k s

generate :: Integer -> M.Map Integer Integer -> Integer
generate 0 mp' = foldr (\(_, s) ac -> ac + s) 0 (M.toList mp')
generate ix mp' = generate (ix -1) (stepMp mp')

part1 :: M.Map Integer Integer -> Integer
part1 = generate 80

part2 :: M.Map Integer Integer -> Integer
part2 = generate 256

prepare :: Num a => [Char] -> M.Map Integer a
prepare s = mp
  where
    mp = M.fromListWith (+) (zip ns (repeat 1))
    ns = map (\s -> read s :: Integer) $ splitOn "," s

main :: IO ()
main = readFile "data/Day6.txt" >>= print . (part1 &&& part2) . prepare
