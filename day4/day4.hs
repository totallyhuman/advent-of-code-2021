import Data.List (inits, intersect, sortOn, transpose)

isBingo :: [Int] -> [[Int]] -> Bool
isBingo nums board = 5 `elem` [length (nums `intersect` l) | l <- board ++ transpose board]

winners :: [Int] -> [[[Int]]] -> [([Int], [[Int]])]
winners nums boards = sortOn (length . fst) [head [(n, b) | n <- drop 5 $ inits nums, isBingo n b] | b <- boards]

score :: ([Int], [[Int]]) -> Int
score (nums, board) = sum [i | r <- board, i <- r, i `notElem` nums] * last nums


part1 :: [Int] -> [[[Int]]] -> Int
part1 nums boards = score $ head $ winners nums boards


part2 :: [Int] -> [[[Int]]] -> Int
part2 nums boards = score $ last $ winners nums boards


parseBoards :: [[Char]] -> [[[Int]]]
parseBoards [] = []
parseBoards (_ : list) = [map read $ words r | r <- board] : parseBoards boards
    where (board, boards) = span (/= "") list


main = do
    input <- lines <$> readFile "input.txt"

    let nums = read $ "[" ++ head input ++ "]"
        boards = parseBoards $ tail input

    print $ part1 nums boards
    print $ part2 nums boards
