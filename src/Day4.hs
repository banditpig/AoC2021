{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4 where
  
import Control.Arrow ((&&&))
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromJust, isNothing)
import InputParsers
import Text.Parsec
import Text.Parsec.String
import Prelude

data Five a = Five a a a a a deriving (Foldable, Functor,Eq, Traversable, Show)
newtype Card a = Card (Five (Five a)) deriving (Show, Eq, Functor, Foldable)
newtype Game a = Game [Card a] deriving (Show)

type Numbers = [Int]
type GameState = (Numbers, Game (Maybe Int))

five :: Parser (Five (Maybe Int))
five = do
  f1 <- many space *> int <* many1 space
  f2 <- many space *> int <* many1 space
  f3 <- many space *> int <* many1 space
  f4 <- many space *> int <* many1 space
  f5 <- many space *> int
  pure $ Five (Just f1) (Just f2) (Just f3) (Just f4) (Just f5)

card :: Parser (Card (Maybe Int))
card = do

  f1 <- five <* newline
  f2 <- five <* newline
  f3 <- five <* newline
  f4 <- five <* newline
  f5 <- five <* newline
  pure . Card $ Five f1 f2 f3 f4 f5

game :: Parser (Game (Maybe Int))
game = do
  cards <- many1 card
  pure . Game $ cards

gameState :: Parser GameState
gameState = do
  numbrs <- many1 (int <* (char ',' <|> newline ))
  gm <- game
  pure (numbrs, gm)

instance Applicative Five where
  pure x = Five x x x x x
  Five fs ft fu fv fw <*> Five s t u v w = Five (fs s) (ft t) (fu u) (fv v) (fw w)

score :: (Num c, Foldable t) => c -> t (Maybe c) -> c
score n cd = (* n) . sum . catMaybes $ foldr (:) [] cd

removeWinners :: Game (Maybe a)  -> Game (Maybe a) 
removeWinners (Game cards) = Game (filter (not.winningCard) cards)
  
checkForWinner :: a -> Game (Maybe a) -> Maybe (Card (Maybe a))
checkForWinner numbr (Game cards) = go numbr cards
  where
    go _ [] = Nothing
    go n (cd : cds)
      | winningCard cd = Just cd
      | otherwise = go n cds

--wins if any of rows all nothing
--or any transposed rows are all nothing
winningCard :: Card (Maybe a) -> Bool
winningCard (Card c) = any (all isNothing) c || any (all isNothing) (sequenceA c)

markAllCards :: (Ord a) => a -> Game (Maybe a) -> Game (Maybe a)
markAllCards _ (Game []) = Game []
markAllCards n (Game cards) = Game $ fmap (markCard n) cards

markCard :: (Ord a) => a -> Card (Maybe a) -> Card (Maybe a)
markCard n card = card'
  where
    card' = fmap check card
      where
        check Nothing = Nothing
        check (Just i) = if i == n then Nothing else Just i


part1 :: (Integral a, Num a, Ord a) => ([a], Game (Maybe a)) -> Maybe a
part1 (nmbrs, game) = go nmbrs game
  where
    go [] _ = Nothing
    go (n : ns) game = winner
      where
        game' = markAllCards n game
        winner = case checkForWinner n game' of
          Nothing   -> go ns game'
          Just card -> Just $ score n card

part2 :: (Ord a, Num a) => ([a], Game (Maybe a)) -> a
part2 (nmbrs, game) = go nmbrs game []
  where
    go [] _ wins = head wins
    go (n : ns) game wins  = winner
      where
        game'  = markAllCards n game
        winner = case checkForWinner n game' of
          Nothing   -> go ns (removeWinners game') wins
          Just card -> go ns (removeWinners game')  ((score n card) : wins) 
           
          
day4 :: IO ()
day4 = withData "data/Day4.txt" gameState >>= print . (part1 &&& part1)
--day4 :: IO ()
--day4 = do
--  gs <- withData "data/Day4.txt" gameState
--
--  print (playGame gs)
--  print (playGame2 gs)

