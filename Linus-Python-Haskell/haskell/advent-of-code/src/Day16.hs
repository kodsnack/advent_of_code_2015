module Day16 (day16_1, day16_2) where

import Data.List.Split
import Data.Map.Lazy ((!), Map, fromList, findWithDefault, toList)
import Debug.Trace

type Aunt = Map String Int
type KnownAunt = Map String (Int->Bool)

toPair :: String -> (String, Int)
toPair property =
  let l = splitOn ":" property
  in (head l, read $ (l !! 1))

parseLine :: String -> Aunt
parseLine line = 
  let auntNumber = (read $ splitOn " " line !! 1) :: Int 
      auntPropertiesString = unwords $ snd $ splitAt 2 $ words line
      auntProperties = splitOn "," $ filter (/=' ') auntPropertiesString
  in fromList $ map toPair auntProperties

parseLines :: String -> [Aunt]
parseLines input = 
  let parsed = map parseLine $ lines input
  in parsed


auntData_day1 :: KnownAunt
auntData_day1 = 
  fromList [ ("children", (==3))
           , ("cats", (==7))
           , ("samoyeds", (==2))
           , ("pomeranians", (==3))
           , ("akitas", (==0))
           , ("vizslas", (==0))
           , ("goldfish", (==5))
           , ("trees", (==3))
           , ("cars", (==2))
           , ("perfumes", (==1))
           ]

auntData_day2 :: KnownAunt
auntData_day2 = 
  fromList [ ("children", (==3))
           , ("cats", (>7))
           , ("samoyeds", (==2))
           , ("pomeranians", (<3))
           , ("akitas", (==0))
           , ("vizslas", (==0))
           , ("goldfish", (<5))
           , ("trees", (>3))
           , ("cars", (==2))
           , ("perfumes", (==1))
           ]

equalProperty :: KnownAunt -> (String, Int) -> Bool
equalProperty known (prop, n) = 
  findWithDefault (==(-1)) prop known n

possibleAunt :: KnownAunt -> Aunt -> Bool
possibleAunt known toTest =
  all (equalProperty known) $ toList toTest

findAunt :: KnownAunt -> String -> String
findAunt knownAunt input = 
  let aunts = zip [1..] $ parseLines input 
      theAunt = head $ filter (possibleAunt knownAunt . snd) aunts
  in show $ fst theAunt

day16_1 :: String -> String
day16_1 = 
  findAunt auntData_day1

day16_2 :: String -> String
day16_2 = 
  findAunt auntData_day2
