import Data.Char (isDigit)
import Data.List (group, sort)

isStraight :: [Int] -> Bool
isStraight [x1, y1, x2, y2] = x1 == x2 || y1 == y2

points :: [Int] -> [(Int, Int)]
points [x1, y1, x2, y2]
    | (x1, y1) /= (x2, y2) = (x1, y1) : points [x1 + signum (x2 - x1), y1 + signum (y2 - y1), x2, y2]
    | otherwise = [(x1, y1)]

part1 :: [[Int]] -> Int
part1 = length . filter ((>= 2) . length) . group . sort . concatMap points . filter isStraight

part2 :: [[Int]] -> Int
part2 = length . filter ((>= 2) . length) . group . sort . concatMap points

parseLine :: String -> [Int]
parseLine "" = []
parseLine line = read num : parseLine (dropWhile (not . isDigit) nums)
    where (num, nums) = span isDigit line

main = do
  input <- map parseLine . lines <$> readFile "input.txt"

  print $ part1 input
  print $ part2 input
