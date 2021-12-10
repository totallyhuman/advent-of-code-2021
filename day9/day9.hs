import Data.List (nub, sortOn)
import Data.Ord (Down (Down))

type Grid = [[Int]]
type Coordinate = (Int, Int)

index :: [[a]] -> Coordinate -> a
index l (x, y) = l !! y !! x

adjacents :: Grid -> Coordinate -> [Coordinate]
adjacents l (x, y) = [(x', y') | (x', y') <- potentials,
                                 x' >= 0, x' < length (head l),
                                 y' >= 0, y' < length l]
    where potentials = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lowPoints :: Grid -> [Coordinate]
lowPoints l = [(x, y) | x <- [0 .. length (head l) - 1],
                        y <- [0 .. length l - 1],
                        and [index l (x, y) < index l (x', y') | (x', y') <- adjacents l (x, y)]]


part1 :: Grid -> Int
part1 l = sum $ map (succ . index l) $ lowPoints l


basin :: Grid -> Coordinate -> [Coordinate]
basin l (x, y) = nub $ basin' l (x, y)
    where basin' l (x, y) = (x, y) : concat [basin l (x', y') | (x', y') <- adjacents l (x, y),
                                                                index l (x, y) < index l (x', y'),
                                                                index l (x', y') < 9]

part2 :: Grid -> Int
part2 l = product $ take 3 $ sortOn Down $ map (length . basin l) $ lowPoints l


parseLine :: [Char] -> [Int]
parseLine = map (read . pure)


main = do
    input <- map parseLine . lines <$> readFile "input.txt"

    print $ part1 input
    print $ part2 input