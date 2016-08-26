module Day1 ( day1_1, day1_2 ) where

import Control.Monad

-- First part
action '(' = 1
action ')' = -1
action _ = 0

solve1 input = foldl (+) (0) $ map action input

day1_1 = show . solve1 


-- Second part

solve2 input = length $ takeWhile (>=0) 
  $ scanl (+) (0) $ map action input

day1_2 = show . solve2
