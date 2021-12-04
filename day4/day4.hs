import Data.List
import Data.Ord

isBingo nums board = any ((== 5) . length . intersect nums) (board ++ transpose board)
winners nums boards = [head [(b, n) | n <- drop 5 $ inits nums, isBingo n b] | b <- boards]

score (b, n) = sum (filter (`notElem` n) $ foldl (++) [] b) * last n

part1 nums boards = score $ minimumBy (comparing (length . snd)) $ winners nums boards
part2 nums boards = score $ maximumBy (comparing (length . snd)) $ winners nums boards

groups _ [] = []
groups n l  = (take n l) : (groups n (drop n l))

parseBoard b = [map (read :: String -> Int) $ words r | r <- b]

main = do
    input <- lines <$> readFile "input.txt"
    nums <- return ((read :: String -> [Int]) $ "[" ++ head input ++ "]")
    boards <- return (map (parseBoard . (take 5)) $ groups 6 $ drop 2 input)

    print $ part1 nums boards
    print $ part2 nums boards
