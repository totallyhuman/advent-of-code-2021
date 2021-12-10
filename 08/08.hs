import Data.List

part1 = sum . map (length . filter ((`elem` [2, 3, 4, 7]) . length) . words . last)

-- length 2: 1
-- length 3: 7
-- length 4: 4
-- length 7: 8
-- length 5:
--     contains all of 1's segments: 3
--     contains all of 4's segments that are not 1's: 5
--     otherwise: 2
-- length 6:
--     contains all of 1's and 5's segments: 9
--     contains all of 7's segments: 0
--     otherwise: 6

isSublistOf x y = all (`elem` y) x

isPermutationOf x y = x `elem` permutations y

getValue list key = snd $ head $ filter ((== key) . fst) list

getKey list value = fst $ head $ filter (isPermutationOf value . snd) list

decode list = length6 $ length5 $ uniqueLengths $ zip (repeat (-1)) list
    where uniqueLengths list = [case length segments of
                                    2 -> (1, segments)
                                    3 -> (7, segments)
                                    4 -> (4, segments)
                                    7 -> (8, segments)
                                    x -> (digit, segments) | (digit, segments) <- list]
          length5 list = [if getValue list 1 `isSublistOf` segments
                          then (3, segments)
                          else if (getValue list 4 \\ getValue list 1) `isSublistOf` segments
                               then (5, segments)
                               else (2, segments) | (digit, segments) <- list, length segments == 5] ++ filter ((/= 5) . length . snd) list
          length6 list = [if (getValue list 1 `union` getValue list 5) `isSublistOf` segments
                          then (9, segments)
                          else if getValue list 7 `isSublistOf` segments
                               then (0, segments)
                               else (6, segments) | (digit, segments) <- list, length segments == 6] ++ filter ((/= 6) . length . snd) list

part2 list = sum [read $ concat [show $ getKey (decode $ words patterns) digit | digit <- words output] | [patterns, output] <- list]

split :: Eq a => [a] -> [a] -> [[a]]
split d s
    | d `isInfixOf` s = take (length x - length d) x : split d (s \\ x)
    | otherwise       = [s]
    where x = head $ dropWhile (not . isSuffixOf d) $ inits s

parseLine = split " | "

main = do
    input <- map parseLine . lines <$> readFile "08.in"

    print $ part1 input
    print $ part2 input