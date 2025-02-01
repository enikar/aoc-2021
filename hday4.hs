-- AoC 2021, day 4,

-- depends: base, containers, attoparsec
-- for attoparsec: array, bytestring, deepseq, ghc-prim,
--                 scientific, text, transformers, containers
-- for containers: array, deepseq, template-haskell

-- solution inspired from:  https://github.com/MondayMorningHaskell/AdventOfCode/blob/main/src/Day4.hs
-- It is *exactly* the same algorithm with these differences:
--   - The solution is computed outside of IO.
--   - We don't use monad-logger at all.
--   - We use containers instead of unordered-containers, so we can use
--     IntMap which are very efficient.
--   - We use attoparsec instead of megaparsec.
--   - We use foldr to loop and short-circuit when the solution
--     is found

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Eta reduce" -}

import Data.List
  (find
  ,partition
  )
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Control.Applicative (optional)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,string
  ,char
  ,many1
  ,sepBy1'
  )

type Coord = (Int, Int)
data Board = Board
  {nrows :: Int -- number of rows in a board
  ,ncols :: Int -- number of columns
  ,rows :: IntMap Int -- number of occupied cases in a row
  ,cols :: IntMap Int -- number of occupied cases in a column
  ,grid :: IntMap Coord  -- Map grid's numbers to their coordinates
  ,unmarked :: Set Coord -- Set of coordinates not yet encounter
  ,hasWon :: Bool        -- Is it a winning board?
  } deriving (Show)


printSolution :: Show a => String -> Maybe a -> IO ()
printSolution part sol =
  let sol' = maybe "No solution found" show sol
  in putStrLn (part <> ": " <> sol')

main :: IO ()
main = do
  (draws, boards) <- getDatas "day4.txt"
  -- we reverse draws, because we use foldr to short-circuit
  let ds = reverse draws
  printSolution "Part1" (part1 ds boards)
  printSolution "Part2" (part2 ds boards)

-- part1 is straightforward. We stop when we encounter
-- the first winner board.
part1 :: [Int] -> [Board] -> Maybe Int
part1 draws bs = getResult (foldr search (Nothing, bs) draws)
  where
    getResult = \case
      (Nothing, _)     -> Nothing
      (Just (n, b), _) -> Just (n * sumGrid b)

    search _ acc@(Nothing, []) = acc -- no solution
    search _ acc@(Just _, _)   = acc -- short circuit
    search d (_, boards)       =
      let boards' = map (playDraw d) boards
      in case find hasWon boards' of
           Nothing -> (Nothing, boards') -- continue to search
           Just board -> (Just (d, board), []) -- found the first winner

-- For part2, we end when there is no more looser boards, "we are
-- sure" we found the last winner. (Well, it is not so sure…)
-- Anyway this is sufficient for a solution for an AoC puzlle
-- because the game has be designed in a such way that it works.
part2 :: [Int] -> [Board] -> Maybe Int
part2 draws bs = getResult (foldr search (Nothing, bs) draws)
  where
    getResult = \case
      (Nothing, _)     -> Nothing
      (Just (n, b), _) -> Just (n * sumGrid b)

    -- search _ acc@(Nothing, []) = acc -- no solution not reached
    search _ acc@(Just _, _)   = acc -- short circuit
    search d (_, boards)
      |null loosers = (Just (d, head winners), []) -- found the last winner.
      |otherwise    = (Nothing, loosers) -- continue with the remaining boards
       where
         boards' = map (playDraw d) boards
         (winners, loosers) = partition hasWon boards'

-- playDraw updates board if needed
playDraw :: Int -> Board -> Board
playDraw d board = case M.lookup d (grid board) of
  Nothing     -> board -- Nothing to update
  Just (c, r) -> board {rows = newRows
                       ,cols = newCols
                       ,unmarked = newUnmarked
                       ,hasWon = occRows == nrows board
                                 || occCols == ncols board
                       }
    where
      -- we found d on the grid so we update occupied
      -- rows and columns
      (newRows, occRows) = updateOcc (rows board) r
      (newCols, occCols) = updateOcc (cols board) c
      -- this coordinate is no longer unmarked
      newUnmarked = S.delete (c, r) (unmarked board)

      -- To update rows and columns
      updateOcc occ n = case M.lookup n occ of
        Nothing -> (M.insert n 1 occ, 1)
        Just x  -> (M.insert n (x+1) occ, x+1)

-- sumGrid sums up all values of unmarked cases.
sumGrid :: Board -> Int
sumGrid board = M.foldlWithKey' f 0 (grid board)
  where
    unms = unmarked board
    f acc key pair
      | pair `S.member` unms = acc + key
      | otherwise                = acc

-- Parsing stuff
getDatas :: String -> IO ([Int], [Board])
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> ([Int], [Board])
parseDatas str =
  either error -- to escape quickly when the parsing is wrong
         id
         (parseOnly parseGame str)

-- parseGame :: Parser ([Int], [Board])
-- parseGame = do
--   numbers <- decimal `sepBy1'` char ','
--   void (string "\n\n")
--   grids <- parseBoard `sepBy1'` string "\n\n"
--   pure (numbers, grids)

-- Alternative version using Applicative, just for fun!
parseGame :: Parser ([Int], [Board])
parseGame =
  liftA2 (,)
         (decimal `sepBy1'` char ',' <* string "\n\n")
         (parseBoard `sepBy1'` string "\n\n")

parseBoard :: Parser Board
parseBoard = makeBoard <$> parseRow `sepBy1'` char '\n'

parseRow :: Parser [Int]
parseRow =
  optional (char ' ')
  *> decimal `sepBy1'` many1 (char ' ')

-- Indices begin at one, not zero. As well, that doesn't matter
-- since we don't use this information.
makeBoard :: [[Int]] -> Board
makeBoard lss = board
  where
    g = [(v, (x, y))
        |(y, xs) <- zip  [1..] lss
        ,(x, v) <- zip [1..] xs
        ]
-- To map numbers to their coordinates, we assume there are
-- no repetitions of a number inside a grid.
-- This is the case. We can check this, the grid's size of
-- each board should be equal to 25.
    board =
      Board {nrows = length lss
            ,ncols = length (head lss)
            ,rows = M.empty
            ,cols = M.empty
            ,grid = M.fromList g
            ,unmarked = S.fromList (map snd g)
            ,hasWon = False
            }
