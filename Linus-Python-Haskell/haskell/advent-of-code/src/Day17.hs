module Day17 (day17_1, day17_2) where

import Data.List.Split


parseLines :: String -> [Int]
parseLines input = map read $ lines input


subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = 
  let ss = subsets xs 
  in ss ++ map (x:) ss


hasSum :: (Eq a, Num a) => a -> [a] -> Bool
hasSum n = (== n) . sum


day17_1 :: String -> String
day17_1 input = 
  let ss = subsets $ parseLines input
  in show $ length $ filter (hasSum 150) ss


hasLength :: Int -> [a] -> Bool
hasLength n = (==n) . length


-- Predicate combiner.
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = f a && g a


day17_2 :: String -> String
day17_2 input = 
  let ss = subsets $ parseLines input
  in show $ head $ filter (/=0) $ map length [ filter (hasSum 150 .&&. hasLength n) ss | n <- [1..] ]
