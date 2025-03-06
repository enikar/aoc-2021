-- AoC 2021, day 2

{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (foldl')

data Instruction = Forward Int
                 | Down Int
                 | Up Int

type Instructions = [Instruction]

getDatas :: String -> IO Instructions
getDatas filename = parseInstructions <$> readFile' filename

parseInstructions :: String -> Instructions
parseInstructions str = map parseInstruction (lines str)
  where
    parseInstruction s = case words s of
      ["forward", n] -> Forward (readInt n)
      ["down", n]   -> Down (readInt n)
      ["up", n]     -> Up (readInt n)
      _             -> error "Error: parseInstruction: invalid input"

    readInt s = fromMaybe errorReadInt (readMaybe s)
      where
        errorReadInt = error ("Error: readInt: not an Int: " <> s)

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

main :: IO ()
main = do
  instrs <- getDatas "day2.txt"
  printSolution "Part1" (part1 instrs)
  printSolution "Part2" (part2 instrs)

part1 :: Instructions -> Int
part1 instrs = x' * z'
  where
    (x',z') = foldl' reduce (0, 0) instrs

    reduce (x, z) = \case
      Forward n -> (x+n, z)
      Down    n -> (x, z+n)
      Up      n -> (x, z-n)

part2 :: Instructions -> Int
part2 instrs = x' * z'
  where
    (x', z', _) = foldl' reduce (0, 0, 0) instrs

    reduce (x, z, a) = \case
        Forward n -> (x+n, z+n*a, a)
        Down    n -> (x, z, a+n)
        Up      n -> (x, z, a-n)
