import Data.List

isBingo nums board = 5 `elem` [length (nums `intersect` l) | l <- board ++ transpose board]

winners nums boards = sortOn (length . snd) [head [(b, n) | n <- drop 5 $ inits nums, isBingo n b] | b <- boards]

score (board, nums) = sum [i | r <- board, i <- r, i `notElem` nums] * last nums

part1 nums boards = score $ head $ winners nums boards

part2 nums boards = score $ last $ winners nums boards

parseBoards [] = []
parseBoards (_ : list) = [map read $ words r | r <- board] : parseBoards boards
  where
    (board, boards) = span (/= "") list

main = do
    input <- lines <$> readFile "input.txt"

    let nums = read $ "[" ++ head input ++ "]"
        boards = parseBoards $ tail input

    print $ part1 nums boards
    print $ part2 nums boards
