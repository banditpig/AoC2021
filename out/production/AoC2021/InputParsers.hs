{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InputParsers where
  
import Data.Text (Text)
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Prelude

withData :: FilePath -> Parser a -> IO a
withData path p = do
  result <- parseFromFile (p ) path
  either (error . show) pure result

parserListString :: Parser [[String]]
parserListString = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

parserListListInt :: Parser [[Int]]
parserListListInt = many1 (sepBy1 number (char '\t') <* newline)
    where
      
        number :: Parser Int
        number = read <$> many1 digit

parserListInt :: Parser [Int]
parserListInt = many ( int <* newline)

int :: Parser Int
int = char '-' *> pure ((-1)*) <*> natural <|> char '+' *> pure ((1)*) <*> natural <|> natural

natural :: Parser Int
natural = pure read <*> many1 digit

keyWord :: String -> Parser String
keyWord = string



commaSp :: Parser String
commaSp = string ", "

names :: Parser [String]
names = do
    nl <- sepEndBy1 (many1 letter) commaSp
    return  nl