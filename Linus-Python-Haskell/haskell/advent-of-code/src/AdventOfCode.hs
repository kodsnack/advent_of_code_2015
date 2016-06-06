{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode
    ( Problem
    , solution
    , inputFile
    , Day
    , InputFile
    , lookupProblem 
    , listProblems
    ) where
    
import qualified Data.Map.Lazy as Map 

import Day1 
import Day12
import Day13

type InputFile = String
type Day = String
data Problem = Problem { inputFile :: String, solution :: String -> String }

problems :: Map.Map Day Problem
problems = 
  Map.fromList [ ("day1-1"
                 , Problem { inputFile = "day1-input.txt", solution = day1_1})
               , ("day1-2"
                 , Problem { inputFile = "day1-input.txt", solution = day1_2})
               , ("day12-2"
                 , Problem { inputFile = "day12-input.txt", solution = day12_2})
               , ("day13-1"
                 , Problem { inputFile = "day13-input.txt", solution = day13_1})
               , ("day13-2"
                 , Problem { inputFile = "day13-input.txt", solution = day13_2})
               ]

lookupProblem day = Map.lookup day problems

listProblems = Map.keys problems

