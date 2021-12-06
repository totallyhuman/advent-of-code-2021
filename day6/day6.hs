collect :: [Int] -> [Int]
collect l = [length $ filter (== i) l | i <- [0 .. 8]]

simulate :: Int -> [Int] -> [Int]
simulate 0 list    = list
simulate days list = simulate (days - 1) list'
    where list' = [case i of
                       8 -> list !! 0 
                       6 -> list !! 0 + list !! 7
                       x -> list !! (x + 1)
                   | i <- [0 .. 8]]


part1 :: [Int] -> Int
part1 = sum . simulate 80 . collect


part2 :: [Int] -> Int
part2 = sum . simulate 256 . collect


main = do
    input <- readFile "input.txt"

    let list = read $ "[" ++ input ++ "]"

    print $ part1 list
    print $ part2 list