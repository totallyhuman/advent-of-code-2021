import Data.List (sort)

data Status = Complete | Incomplete String | Corrupted Char

checkSyntax :: String -> String -> Status
checkSyntax "" "" = Complete
checkSyntax d  "" = Incomplete d
checkSyntax d (c : l)
    | c == '('    = checkSyntax (')' : d) l
    | c == '['    = checkSyntax (']' : d) l
    | c == '{'    = checkSyntax ('}' : d) l
    | c == '<'    = checkSyntax ('>' : d) l
    | c == head d = checkSyntax (tail d) l
    | otherwise   = Corrupted c


part1 :: [String] -> Int
part1 l = sum [points i | Corrupted i <- map (checkSyntax "") l]
    where points ')' = 3
          points ']' = 57
          points '}' = 1197
          points '>' = 25137


part2 :: [String] -> Int
part2 l = scores !! (length scores `div` 2)
    where points ')' = 1
          points ']' = 2
          points '}' = 3
          points '>' = 4

          scores = sort [foldl (\a i -> a * 5 + points i) 0 i | Incomplete i <- map (checkSyntax "") l]


main = do
    input <- lines <$> readFile "10.in"

    print $ part1 input
    print $ part2 input