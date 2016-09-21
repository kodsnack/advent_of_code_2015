module Day18 (day18_1, day18_2) where

import Data.Array as A

type Grid = A.Array (Int,Int) Int
type Kernel = Grid -> (Int,Int) -> Int

-- Parsing
toInt '#' = 1
toInt _   = 0

parseLines :: String -> [[Int]]
parseLines input = map (map toInt) (lines input)

gridWidth :: [[Int]] -> Int
gridWidth grid = length $ head grid

toArray :: [[Int]] -> Grid
toArray grid =
  let width = gridWidth grid
  in A.listArray ((1, 1), (gridWidth grid, length grid)) (concat grid)

wrapGrid :: Grid -> Grid
wrapGrid grid =
  let ((iLo, jLo),(iHi, jHi)) = bounds grid
  in A.array ((iLo-1, jLo-1), (iHi+1, jHi+1)) [ ((i,j),0) | i <- [iLo-1..iHi+1], j <- [jLo-1..jHi+1] ] // A.assocs grid

-- Game of Life
neighbors :: Grid -> (Int, Int) -> Int
neighbors grid (i, j) = grid ! (i-1, j+1) + grid ! (i, j+1) + grid ! (i+1, j+1) +
                        grid ! (i-1, j)           +           grid ! (i+1, j)   +
                        grid ! (i-1, j-1) + grid ! (i, j-1) + grid ! (i+1, j-1)

update :: Int -> Int -> Int
update 0 3 = 1
update 1 2 = 1
update 1 3 = 1
update _ _ = 0

gameOfLifeKernel grid pos = update (grid ! pos) (neighbors grid pos)

-- Processing
realBounds :: Grid -> ((Int,Int),(Int,Int))
realBounds grid =
  let ((iLo, jLo), (iHi, jHi)) = A.bounds grid
  in ((iLo+1, jLo+1),(iHi-1, jHi-1))

updateGrid :: Kernel -> Grid -> Grid
updateGrid kernel grid =
  let ((iLo, jLo), (iHi, jHi)) = realBounds grid
  in grid // [ ((i,j), kernel grid (i,j)) | i <- [iLo..iHi] , j <- [jLo..jHi] ]

count grid = sum (A.elems grid)

day18_1 :: String -> String
day18_1 input =
  let grid = wrapGrid $ toArray $ parseLines input
  in show $ count (foldl (\g _ -> updateGrid gameOfLifeKernel g) grid [1..100])

-- Problem 2
turnOnCorners :: Grid -> Grid
turnOnCorners grid =
  let ((iLo, jLo), (iHi, jHi)) = realBounds grid
  in grid // [ ((iLo,jLo), 1), ((iLo,jHi), 1), ((iHi,jLo), 1), ((iHi,jHi), 1) ]

day18_2 :: String -> String
day18_2 input =
  let grid = wrapGrid $ toArray $parseLines input
  in show $ count (foldl (\g _ -> (turnOnCorners . updateGrid gameOfLifeKernel) g) (turnOnCorners grid) [1..100])
