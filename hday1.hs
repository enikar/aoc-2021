-- AoC 2021, day 1

{- HLINT ignore "Eta reduce" -}


import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO (readFile')
import Data.List (tails)


getDatas :: String -> IO [Int]
getDatas filename = map readInt . lines <$> readFile' filename
  where
    readInt s = fromMaybe errorGetDatas (readMaybe s)
    errorGetDatas = error "Error: getDatas: not an Int"

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  datas <- getDatas "day1.txt"
  printSolution "Part1" (part1 datas)
  printSolution "Part2" (part2 datas)


count :: (Int -> Bool) -> [Int] -> Int
count p = length . filter p

part1 :: [Int] -> Int
part1 xs = count (>0) (zipWith subtract xs (tail xs))

part2 :: [Int] -> Int
part2 xs = part1 (map (sum . take 3) (tails xs))
