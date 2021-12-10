import Data.Char (isDigit)
import Data.List (group, inits, isInfixOf, isSuffixOf, sort, (\\))

type Point = (Int, Int)
type Line = (Point, Point)

isAxisAligned :: Line -> Bool
isAxisAligned ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

points :: Line -> [Point]
points ((x1, y1), (x2, y2)) = zip [x1, xd .. x2] [y1, yd .. y2]
    where xd = x1 + signum (x2 - x1)
          yd = y1 + signum (y2 - y1)


part1 :: [Line] -> Int
part1 = length . filter ((>= 2) . length) . group . sort . concatMap points . filter isAxisAligned


part2 :: [Line] -> Int
part2 = length . filter ((>= 2) . length) . group . sort . concatMap points


split :: Eq a => [a] -> [a] -> [[a]]
split d s
    | d `isInfixOf` s = take (length x - length d) x : split d (s \\ x)
    | otherwise       = [s]
    where x = head $ dropWhile (not . isSuffixOf d) $ inits s

parseLine :: String -> Line
parseLine line = ((x1, y1), (x2, y2))
    where [[x1, y1], [x2, y2]] = map (map read . split ",") $ split " -> " line


main = do
    input <- map parseLine . lines <$> readFile "05.in"

    print $ part1 input
    print $ part2 input
