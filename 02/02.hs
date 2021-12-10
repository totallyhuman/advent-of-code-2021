part1f :: [String] -> Int -> Int -> Int
part1f [] pos dep = pos * dep
part1f (com : coms) pos dep
    | dir == "forward" = part1f coms (pos + x) dep
    | dir == "up"      = part1f coms pos (dep - x)
    | dir == "down"    = part1f coms pos (dep + x)
    where dir = head $ words com
          x   = read $ last $ words com

part1 :: [String] -> Int
part1 list = part1f list 0 0


part2f :: [String] -> Int -> Int -> Int -> Int
part2f [] pos dep aim = pos * dep
part2f (com : coms) pos dep aim
    | dir == "forward" = part2f coms (pos + x) (dep + aim * x) aim
    | dir == "up"      = part2f coms pos dep (aim - x)
    | dir == "down"    = part2f coms pos dep (aim + x)
    where dir = head $ words com
          x   = read $ last $ words com

part2 :: [String] -> Int
part2 list = part2f list 0 0 0


main = do
    input <- lines <$> readFile "02.in"
    print $ part1 input
    print $ part2 input
