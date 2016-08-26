{-# LANGUAGE OverloadedStrings #-}
module Day12 ( day12_2 ) where

import Text.ParserCombinators.Parsec
import Control.Monad

data V = Array [V] 
       | Object V 
       | Number Integer 
       | Red 
       | RedObject
       | String String deriving (Show,Eq)

parseKeyValue :: Parser V
parseKeyValue = do 
                  key <- parseString
                  char ':'
                  spaces
                  value <- parseValue
                  if value == Red then 
                    return Red else 
                    return $ Object value
                  
parseNumber :: Parser V
parseNumber = do
  x <- many1 (digit <|> (char '-'))
  return $ Number (read x)


parseString :: Parser V
parseString = do 
  char '"'
  x <- many $ noneOf "\""
  char '"'
  if x == "red" then 
    return Red else 
    return $ String x

parseArray :: Parser V
parseArray = do
  char '['
  values <- parseValue `sepBy` (char ',')
  char ']'
  return $ Array values


parseObject :: Parser V
parseObject = do 
  char '{'
  x <- parseKeyValue `sepBy` (char ',')
  char '}'
  if Red `elem` x then 
     return RedObject else
     return $ Array x


parseValue :: Parser V
parseValue = parseObject <|> parseArray <|> parseNumber <|> parseString

countNumbers :: V -> Integer
countNumbers v = case v of Number n -> n
                           Array a -> foldr (+) 0 $ map countNumbers a
                           Object o -> countNumbers o
                           _ -> 0

day12_2 :: String -> String
day12_2 input = 
  let object = parse parseObject "Parse failed" input 
  in case object of 
     Left err -> ("ERROR: " ++ (show err))
     Right val -> show $ countNumbers val
