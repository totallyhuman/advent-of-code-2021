part1f [] pos dep = pos * dep
part1f (com : coms) pos dep
    | com0 == 'f' = part1f coms (pos + x) dep
    | com0 == 'u' = part1f coms pos (dep - x)
    | com0 == 'd' = part1f coms pos (dep + x)
    where com0 = head com
          x    = read $ last $ words $ com

part1 list = part1f list 0 0

part2f [] pos dep aim = pos * dep
part2f (com : coms) pos dep aim
    | com0 == 'f' = part2f coms (pos + x) (dep + aim * x) aim
    | com0 == 'u' = part2f coms pos dep (aim - x)
    | com0 == 'd' = part2f coms pos dep (aim + x)
    where com0 = head com
          x    = read $ last $ words $ com

part2 list = part2f list 0 0 0

main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
