import Data.ByteString.Lazy.Char8 hiding (dropWhile, head, map, last, repeat, take, takeWhile, zip, zipWith)
import Data.Digest.Pure.MD5

input = "bgvyzdsv"

numbers :: [String]
numbers = map show [1..]

hashes :: [String]
hashes = map (show . md5 . pack) $ zipWith (++) (repeat input) numbers

solveWith condition = head $ dropWhile (not.condition) $ zip numbers hashes

day4_part1 = fst $ solveWith fiveZeros
    where
        fiveZeros (n, ('0':'0':'0':'0':'0':hs)) = True
        fiveZeros _ = False

day4_part2 = fst $ solveWith sixZeros
    where
        sixZeros (n, ('0':'0':'0':'0':'0':'0':hs)) = True
        sixZeros _ = False

