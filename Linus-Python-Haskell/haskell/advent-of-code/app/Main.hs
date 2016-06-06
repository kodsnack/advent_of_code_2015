module Main where

import Data.List (uncons)
import Control.Monad 
import System.Environment
import AdventOfCode


getProblem :: Day -> IO Problem
getProblem day = 
  let problem = lookupProblem day
  in case problem of 
    Just a -> return a
    Nothing -> ioError $ userError $ "No solution for " ++ day


listSolutions :: IO ()
listSolutions = do
  putStrLn "You need to tell me which problem to solve"
  putStrLn "Choose from the following:"
  foldM (\x y -> putStrLn y) () listProblems 


readArgument :: IO ()
readArgument = do
  args <- getArgs
  case uncons args of 
    Just (x,_) -> solve x
    Nothing -> listSolutions

solve :: Day -> IO ()
solve day = do 
  problem <- getProblem day
  input <- readFile $ inputFile problem 
  putStrLn $ solution problem $ input

main :: IO ()
main = readArgument
