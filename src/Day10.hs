{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import Control.Arrow ((&&&))
import Data.List
import Data.Maybe (mapMaybe, isNothing)
import InputParsers
import Stack
import Text.Parsec
import Text.Parsec.String
import Prelude

data Symbol = Oparen | Cparen | Osquare | Csquare | Obrace | Cbrace | Oarrow | Carrow
  deriving (Show, Eq, Enum)

type LineSymbols = [Symbol]
type Input = [LineSymbols]

symbolChars :: [Char]
symbolChars = ['(', ')', '[', ']', '{', '}', '<', '>']

symbol :: Parser Symbol
symbol = do
  ch <- char '(' <|> char ')' <|> char '[' <|> char ']' <|> char '{' <|> char '}' <|> char '<' <|> char '>'
  pure $ charToSym ch

line :: Parser LineSymbols
line = many symbol <* newline

allLines :: Parser [LineSymbols]
allLines = many line

matchingSymbols :: Symbol -> Symbol -> Bool
matchingSymbols l r
  | (l, r) == (Oparen, Cparen) = True
  | (l, r) == (Osquare, Csquare) = True
  | (l, r) == (Obrace, Cbrace) = True
  | (l, r) == (Oarrow, Carrow) = True
  | otherwise = False

charToSym :: Char -> Symbol
charToSym c = maybe Oparen toEnum (elemIndex c symbolChars)

isOpenSymbol :: Symbol -> Bool
isOpenSymbol s = s `elem` [Oparen, Osquare, Obrace, Oarrow]

cost :: Num p => Symbol -> p
cost = \case
  Cparen -> 3
  Csquare -> 57
  Cbrace -> 1197
  Carrow -> 25137


tailCost :: Symbol -> Int
tailCost = \case
  Oparen -> 1
  Osquare -> 2
  Obrace -> 3
  Oarrow -> 4
  _      -> 0

processLine :: [Symbol] -> Maybe Symbol
processLine ls = go ls empty Nothing
  where
    go [] _ Nothing = Nothing
    go [] _ smthing = smthing
    go (sym : syms) st err
      | isOpenSymbol sym = go syms (push sym st) err
      | otherwise = goagain
      where
        (myb, stack') = pop st
        goagain = case myb of
          Nothing -> Nothing
          Just s ->
            if matchingSymbols s sym then go syms stack' err else Just sym

part1 :: Num a => [[Symbol]] -> a
part1 inp = total
  where
    errs = mapMaybe processLine inp
    total = foldr (\sym acc -> acc + cost sym) 0 errs

stackALine :: [Symbol] -> Stack Symbol
stackALine lin = go lin empty where
  go [] st = st
  go (s:ss)  st
   | isOpenSymbol s = go ss (push s st)
   | otherwise = go ss st' where (_, st') = pop st

part2 :: [[Symbol]] -> Int
part2 inp = midCost   where
  -- stacks for all ok lines
  stacks = map stackALine $ filter (isNothing . processLine) inp
  allTails = map drain stacks
  costs = sort $ map ( foldl (\acc sym -> acc * 5 + tailCost sym)   0 ) allTails
  midCost = (!!) costs  ((length costs)  `div` 2)

main :: IO ()
main = withData "data/Day10.txt" allLines >>= print . (part1 &&& part2)

