-- AoC 2021, day 7

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List.Extra
  (splitOn
  ,trimEnd -- to removed an eventually end of line.
  )

getDatas :: String -> IO [Int]
getDatas filename = parse <$> readFile' filename
  where
    parse str = map readInt (splitOn "," (trimEnd str))
    readInt s = fromMaybe errReadInt (readMaybe s)
      where
        errReadInt = error ("Error: readInt: not an Int: " <> s)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

main :: IO ()
main = do
  datas <- getDatas "day7.txt"
  printSolution "Part1" (part1 datas)
  printSolution "Part2" (part2 datas)

part1 :: [Int] -> Int
part1 nums = minimum (map sumCosts nums)
  where
    sumCosts n = sum (map (cost n) nums)

    cost n x = abs (n - x)

part2 :: [Int] -> Int
part2 nums = minimum (map sumCosts nums)
  where
    sumCosts n = sum (map (cost n) nums)

    cost n x = m * (m+1) `quot` 2
      where
        m = abs (n - x)
