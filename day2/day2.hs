readX = (read :: String -> Int) . last . words

part1f [] pos dep = pos * dep
part1f (('f' : com) : tail) pos dep = part1f tail (pos + readX com) dep
part1f (('u' : com) : tail) pos dep = part1f tail pos (dep - readX com)
part1f (('d' : com) : tail) pos dep = part1f tail pos (dep + readX com)

part1 list = part1f list 0 0

part2f [] pos dep aim = pos * dep
part2f (('f' : com) : tail) pos dep aim = part2f tail (pos + readX com) (dep + aim * readX com) aim
part2f (('u' : com) : tail) pos dep aim = part2f tail pos dep (aim - readX com)
part2f (('d' : com) : tail) pos dep aim = part2f tail pos dep (aim + readX com)

part2 list = part2f list 0 0 0

main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
