part1 :: [Int] -> Int
part1 list = length $ filter id $ zipWith (<) list (tail list)


part2 :: [Int] -> Int
part2 list = length $ filter id $ zipWith (<) list (drop 3 list)


main = do
    input <- map read . lines <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
