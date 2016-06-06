module Day13 ( day13_1 , day13_2) where

import Data.List.Split
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!), Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Node = String
type Edge = ((String, String), Int)
type Graph = Map (String, String) Int


getWeight :: Graph -> String -> String -> Int
getWeight g s t 
  | Map.notMember (s,t) g = 0
  | Map.notMember (t,s) g = 0 -- Not necessary, but looks more complete
  | otherwise = (g ! (s,t)) + (g ! (t,s))

parseLine :: String -> Edge
parseLine str =
  let parts          = splitOneOf " ." str
      from           = head parts
      to             = parts !! 10
      unsignedWeight = read (parts !! 3)
      weight         = if parts !! 2 == "gain" then
                          unsignedWeight else 
                          -unsignedWeight
  in ((from, to), weight)
      
getGraph :: String -> Graph
getGraph input = Map.fromList $ map parseLine $ lines input


getNodes :: String -> Set Node
getNodes input = 
  let getNode str = Set.insert (head $ splitOn " " str) :: Set Node -> Set Node
  in foldr getNode Set.empty $ lines input


-- Looks like a travelling salesman? Anyways bruteforce DFS!!
-- Can be called with lastNode = "" in first round,    
-- thisWeight will be 0 and we calculate the cycle "" -> 1 -> 2 -> .. -> 1
maxPath :: Graph -> Set Node -> Node -> Node -> Int
maxPath graph nodes lastNode startNode = 
  let reducer node maxVal = 
        let thisWeight = getWeight graph lastNode node
            nodesLeft = Set.delete startNode $ Set.delete node nodes
        in max maxVal (thisWeight + maxPath graph nodesLeft node startNode)
  in if Set.null nodes then 
        getWeight graph startNode lastNode else 
        foldr reducer (minBound :: Int) nodes

day13_1 input = 
  let graph = getGraph input
      nodes = getNodes input
  in show $ maxPath graph nodes "" $ Set.findMin nodes -- Any startnode is fine

day13_2 input = 
  let graph = getGraph input
      nodes = Set.insert "mySelf" $ getNodes input -- Unknown nodes -> getWeight = 0
  in show $ maxPath graph nodes "" $ Set.findMin nodes
