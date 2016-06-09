module Day14 ( day14_1, day14_2) where

import Data.List.Split

data State = Flying | Resting deriving (Show)
data Reindeer = Reindeer { speed :: Int
                         , flyTime :: Int
                         , restTime :: Int 
                         , state :: State
                         , distance :: Int
                         } deriving (Show)
data RacingReindeer = RacingReindeer { reindeer :: Reindeer, raceTime :: Int }

nextState :: State -> State
nextState Flying = Resting
nextState Resting = Flying
  
parseLine :: String -> Reindeer
parseLine line = 
  let parts = splitOn " " line
      speed = read $ parts !! 3
      flyTime = read $ parts !! 6
      restTime = read $ parts !! 13
  in Reindeer { speed = speed
              , flyTime = flyTime
              , restTime = restTime 
              , state = Resting
              , distance = 0
              }

parseLines :: String -> [Reindeer]
parseLines input = map parseLine $ lines input

fly :: RacingReindeer -> RacingReindeer 
fly racingReindeer =
  let theReindeer = reindeer racingReindeer 
      flightUntilRest = speed theReindeer * flyTime theReindeer
      flightUntilEndOfRace = speed theReindeer * raceTime racingReindeer
      newLength = distance theReindeer + min flightUntilRest flightUntilEndOfRace
      newReindeer = theReindeer { distance = newLength, state = Flying }
      newRaceTime = max 0 (raceTime racingReindeer - flyTime theReindeer)
  in RacingReindeer { reindeer = newReindeer, raceTime = newRaceTime }

rest racingReindeer = 
  let theReindeer = reindeer racingReindeer
      newRaceTime = max 0 (raceTime racingReindeer - restTime theReindeer)
      newReindeer = theReindeer { state = Resting }
  in RacingReindeer { reindeer = newReindeer, raceTime = newRaceTime }

race :: RacingReindeer -> RacingReindeer
race racingReindeer | raceTime racingReindeer <= 0 = racingReindeer
race racingReindeer = 
  let theState = state $ reindeer racingReindeer
  in case theState of 
    Resting -> race $ fly racingReindeer
    Flying  -> race $ rest racingReindeer

totalRaceTime = 2503 :: Int

initRaceReindeer :: Int -> Reindeer -> RacingReindeer
initRaceReindeer raceTime reindeer = 
  RacingReindeer { raceTime = raceTime, reindeer = reindeer } 

-- Try some hacky point-free stuff
getDistance :: Int -> Reindeer -> Int
--getDistance = 
--  ((distance . reindeer . race) .) . initRaceReindeer
getDistance time theReindeer = (distance . reindeer . race) (initRaceReindeer time theReindeer)

leaderDistance reindeers raceTime = 
  maximum $ map (getDistance raceTime) reindeers

day14_1 :: String -> String 
day14_1 input = 
  let reindeers = parseLines input
  in show $ leaderDistance reindeers totalRaceTime

leaderDistanceAtTime :: [Reindeer] -> [Int]
leaderDistanceAtTime reindeers = 
  [ leaderDistance reindeers time | time <- [1..totalRaceTime] ]

reindeerDistanceAtTime reindeer =
  [ getDistance time reindeer | time <- [1..totalRaceTime] ]

getPoints :: [Reindeer] -> Reindeer -> Int
getPoints reindeers reindeer =
  sum $ map (\(x,y) -> if x==y then 1 else 0 ) 
      $ zip (leaderDistanceAtTime reindeers) (reindeerDistanceAtTime reindeer)

winnerPoints reindeers = maximum $ map (getPoints reindeers) reindeers

day14_2 input = 
  let reindeers = parseLines input
  in show $ winnerPoints reindeers
