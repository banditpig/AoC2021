{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import Control.Arrow ((&&&))
import Data.List
import InputParsers
import Stack
import Text.Parsec
import Text.Parsec.String
import Prelude

--If a chunk opens with (, it must close with ).
--If a chunk opens with [, it must close with ].
--If a chunk opens with {, it must close with }.
--If a chunk opens with <, it must close with >.
data Symbol = Oparen | Cparen | Osquare | Csquare | Obrace | Cbrace | Oarrow | Carrow
  deriving (Show, Eq, Enum)

symbolChars = ['(', ')', '[', ']', '{', '}', '<', '>']

type LineSymbols = [Symbol]

type Input = [LineSymbols]

symbol :: Parser Symbol
symbol = do
  ch <- char '(' <|> char ')' <|> char '[' <|> char ']' <|> char '{' <|> char '}' <|> char '<' <|> char '>'
  pure $ charToSym ch

line :: Parser LineSymbols
line = many symbol <* newline

allLines = many line

-- Just does from Ox -> Cx
flipSym :: Symbol -> Symbol
flipSym s = toEnum $ (fromEnum s) + 1

matchingSymbols :: Symbol -> Symbol -> Bool
matchingSymbols l r
  | (l, r) == (Oparen, Cparen) = True
  | (l, r) == (Osquare, Csquare) = True
  | (l, r) == (Obrace, Cbrace) = True
  | (l, r) == (Oarrow, Carrow) = True
  | otherwise = False

-- Oparen | Cparen | Osquare | Csquare | Obrace | Cbrace | Oarrow | Carrow
-- Oparen | Cparen | Osquare | Csquare | Obrace | Cbrace | Oarrow | Carrow
charToSym :: Char -> Symbol
charToSym c = maybe Oparen toEnum (elemIndex c symbolChars)

isOpenSymbol :: Symbol -> Bool
isOpenSymbol s = s `elem` [Oparen, Osquare, Obrace, Oarrow]

--processLine :: LineSymbols -> (Stack Symbol, Stack Symbol )
-- processLine :: [Symbol] -> (Stack Symbol, Stack Symbol)
-- processLine :: [Symbol] -> Symbol
processLine ls = go ls empty [] 
  where
    go [] _ [] = []
    go [] st [actual] = [actual]
    go (sym : syms) st err
      | isOpenSymbol sym = go syms (push sym st) err
      | otherwise = goagain
      where
        (myb, stack') = pop st
        goagain = case myb of
          Nothing -> []
          Just s ->
            if matchingSymbols s sym
              then go syms stack' err
              else [sym]

--    ): 3 points.
--    ]: 57 points.
--    }: 1197 points.
--    >: 25137 points.
--part1 :: Input -> ()
part1 inp = total
  where
    errs = concat $ map processLine inp
    total = foldr (\sym acc -> acc + cost sym) 0 (errs)
      where
        cost Cparen = 3
        cost Csquare = 57
        cost Cbrace = 1197
        cost Carrow = 25137

part2 = const ()

main :: IO ()
main = withData "data/Day10.txt" allLines >>= print . (part1 &&& part2)

test :: IO ()
test = do
  l <- withData "data/Day10.txt" allLines
  let x = map processLine l
  print x
