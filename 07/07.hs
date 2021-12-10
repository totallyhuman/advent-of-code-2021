part1 :: [Int] -> Int
part1 l = minimum [sum [abs $ j - i | j <- l] | i <- [minimum l .. maximum l]]


part2 :: [Int] -> Int
part2 l = minimum [sum [f $ abs $ j - i | j <- l] | i <- [minimum l .. maximum l]]
    where f x = x * (x + 1) `div` 2


main = do
    input <- readFile "07.in"

    let list = read $ "[" ++ input ++ "]"

    print $ part1 list
    print $ part2 list