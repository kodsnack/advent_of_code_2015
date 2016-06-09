module Day15 (day15_1, day15_2) where

import Data.List.Split
import Data.List

type Ingredient = [Int]
type Receipe = [Int]
type CalorieContent = [Int]


parseLine :: String -> Ingredient
parseLine line = 
  let parts = splitOneOf " ," line
  in map read [parts !! 2, parts !! 5, parts !! 8, parts !! 11, parts !! 14]

parseLines :: String -> ([Ingredient], CalorieContent)
parseLines input = 
  let parsed = map parseLine $ lines input
      ingredients = map (take 4) parsed
      calorieContent = map (!!4) parsed
  in (ingredients, calorieContent)

-- Only handles exactly four ingredients
createReceipe n = 
  let r = map (+(1 :: Int)) [rem n 100, rem (quot n 100) 100, quot n 10000]
  in 100 - sum r : r

isReceipe = 
  all (>0)

-- Knapsack? (NP) Generate all possible receipes
allReceipes :: [Receipe]
allReceipes = 
  filter isReceipe $ map createReceipe [1..1000000]

receipeScore :: [Ingredient] -> Receipe -> Int
receipeScore ingredients receipe = 
  let ingredientScore ingredient = max 0 $ sum $ zipWith (*) ingredient receipe
  in product $ map ingredientScore (transpose ingredients)

receipeHasCalories :: Int -> CalorieContent -> Receipe -> Bool
receipeHasCalories calories calorieContent receipe =
  calories == sum (zipWith (*) calorieContent receipe)

day15_1 :: String -> String
day15_1 input = 
  let (ingredients, _) = parseLines input
  in show $ maximum $ map (receipeScore ingredients) allReceipes

day15_2 :: String -> String
day15_2 input =
  let (ingredients, calorieContent) = parseLines input
      recepiesWith500Calories = filter (receipeHasCalories 500 calorieContent) allReceipes
  in show $ maximum $ map (receipeScore ingredients) recepiesWith500Calories
  
