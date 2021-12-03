import Data.List
import Data.Ord

binToDec "" = 0
binToDec x  = 2 * binToDec (init x) + read [last x]

mostCommon = head . maximumBy (comparing length) . group . sort
leastCommon = head . minimumBy (comparing length) . group . sort

gamma = map mostCommon . transpose
epsilon = map leastCommon . transpose

part1 list = binToDec (gamma list) * binToDec (epsilon list)

oxyGenerator [a] _    = a
oxyGenerator list bit = oxyGenerator [i | i <- list, i !! bit == mostCommon ((transpose list) !! bit)] (bit + 1)

co2Scrubber [a] _    = a
co2Scrubber list bit = co2Scrubber [i | i <- list, i !! bit == leastCommon ((transpose list) !! bit)] (bit + 1)

part2 list = binToDec (oxyGenerator list 0) * binToDec (co2Scrubber list 0)

main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input
    print $ part2 input
