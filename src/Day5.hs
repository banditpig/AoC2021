{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Day5 where
import Prelude
import Data.Map (fromListWith, toList)
import Control.Arrow ((&&&))
import InputParsers
import Text.Parsec
import Text.Parsec.String



data Point a = Point a a  deriving (Show, Eq, Ord)
type Line a = [Point a]
type Input = [[Point Int]]
-- Parsing
pointAndPoint ::Parser  [Point Int]
pointAndPoint = do
  p1 <- int <* char ','
  p2 <- int <* space <* char '-' <* char '>' <* space
  p3 <- int <* char ','
  p4 <- int <* newline
  pure [Point p1 p2, Point p3 p4]

allPoints :: Parser [[Point Int]]
allPoints = do
   allPts <- many1 pointAndPoint
   pure allPts

---- end parsers

linePoints :: (Ord a, Enum a) => PointsFunc a -> Point a -> Point a -> [Point a]
linePoints pf  (Point x y) (Point x' y')
  | x == x' = [Point x ys | ys <- [min y y' .. max y y']]
  | y == y' = [Point xs y | xs <- [min x x' .. max x x']]
  | otherwise  = pf (Point x y) (Point x' y') 

type PointsFunc a = Point a -> Point a -> [Point a]

diagPts :: (Ord a, Num a, Enum a) => Point a -> Point a -> [Point a]
diagPts (Point x y) (Point x' y') = zipWith makePoint [x, (x + dx) .. x'] [y, (y + dy) .. y'] where
 dx =  if x > x' then (-1) else 1
 dy =  if y > y' then (-1) else 1


makePoint :: a -> a -> Point a
makePoint  = Point

frequency :: Ord a => [Point a ] -> [(Point a , Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])


part1 :: (Ord a, Foldable t, Num a, Enum a) => t [Point a] -> Int
part1 pairsPoints = length $ filter (\(p, cnt) -> cnt >= 2) fr where 
  pf _ _ = []
  fr =frequency $  foldr (\[p1,p2] acc  -> acc <>  linePoints pf p1 p2 ) [] pairsPoints

part2 :: (Ord a, Foldable t, Num a, Enum a) => t [Point a] -> Int
part2 pairsPoints = length $ filter (\(p, cnt) -> cnt >= 2) fr where 
  pf = diagPts 
  fr =frequency $  foldr (\[p1,p2] acc  -> acc <>  linePoints pf p1 p2 ) [] pairsPoints 


day5 :: IO ()
day5 = withData "data/Day5.txt" allPoints >>= print . (part1 &&& part2)

