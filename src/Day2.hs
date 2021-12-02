{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Day2 where

import Prelude
import InputParsers
import Text.Parsec
import Text.Parsec.String

-- x, y and aim
type XYA a = (a, a, a)
data UDF  a    = U a | D a | F a deriving Show

runCmdP1 :: (Num a) => XYA a -> UDF a ->  XYA a
runCmdP1 (x, y, _) udf  = case udf of
  U n ->  (x, y - n, 0)
  D n ->  (x, y + n, 0)
  F n ->  (x + n, y, 0)
  
runCmdP2 :: (Num a) =>  XYA a -> UDF a -> XYA a
runCmdP2 (x, y, aim) udf  = case udf of
  U n ->  (x, y , aim - n)
  D n ->  (x, y , aim + n)
  F n ->  (x + n, y + (aim * n), aim)
  
runCommands :: (Num a) => (XYA a  -> UDF a -> XYA a) -> XYA a -> [UDF a]  -> XYA a
runCommands  = foldl 
 
  
udf :: Parser (UDF Int)
udf = do 
  txt <- (keyWord "forward" <|> keyWord "down" <|> keyWord "up" ) <* space
  nmbr <- int <* newline
  case txt of 
    "forward" -> pure (F nmbr)
    "down"    -> pure (D nmbr)
    "up"      -> pure (U nmbr)

allUDF :: Parser [UDF Int]
allUDF = many udf

day2 :: IO ()
day2 = do
  cmds  <- withData "data/Day2.txt" allUDF
  let (x,y, _) = (runCommands runCmdP1 (0, 0, 0) cmds)
  print (x * y)

  let (x,y, _) = (runCommands runCmdP2 (0, 0, 0) cmds)
  print (x * y)