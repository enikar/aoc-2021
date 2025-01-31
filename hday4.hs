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
--   - The dimensions of a Grid is harcoded to 5x5 (function playDraw)

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
type Grid = [(Int, Coord)]
data Board = Board
  {rows :: IntMap Int -- number of occupied cases in a row
  ,cols :: IntMap Int -- number of occupied cases in a column
  ,grid :: IntMap Coord  -- Map grid's numbers to their coordinates
  ,unMarked :: Set Coord -- Set of coordinate not yet encounter
  ,hasWon :: Bool        -- Is it a winning board
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
    search d (v, boards)       =
      let boards' = map (playDraw d) boards
      in case find hasWon boards' of
           Nothing -> (v, boards') -- continue to search
           Just board -> (Just (d, board), []) -- found the first winner

-- For part2, we end when there is no more looser boards, "we are
-- sure" we found the last winner. (Well, not so sure…)
-- Anyway this is sufficient for a solution for an AoC puzlle.
part2 :: [Int] -> [Board] -> Maybe Int
part2 draws bs = getResult (foldr search (Nothing, bs) draws)
  where
    getResult = \case
      (Nothing, _)     -> Nothing
      (Just (n, b), _) -> Just (n * sumGrid b)

    search _ acc@(Nothing, []) = acc -- no solution
    search _ acc@(Just _, _)   = acc -- short circuit
    search d (v, boards)
      |null loosers = (Just (d, head winners), []) -- found the last winner
      |otherwise    = (v, loosers) -- continue with the remaining boards
       where
         boards' = map (playDraw d) boards
         (winners, loosers) = partition hasWon boards'

-- playDraw updates board if need
playDraw :: Int -> Board -> Board
playDraw d board = case M.lookup d (grid board) of
  Nothing -> board -- Nothing to update
  Just (c, r) -> board {rows = newRows
                       ,cols = newCols
                       ,unMarked = newUnMarked
                       -- well, the dimensions of grids are hardcoded,
                       -- it is somewhat ugly…
                       ,hasWon = occRows == 5 || occCols == 5
                       }
    where
      -- we found d on the grid so we update occupied
      -- rows and columns
      (newRows,occRows) = updateOcc (rows board) r
      (newCols, occCols) = updateOcc (cols board) c
      -- this coordinate is no longueur unMarked
      newUnMarked = S.delete (c, r) (unMarked board)

      -- To update rows and columns
      updateOcc occ n = case M.lookup n occ of
        Nothing -> (M.insert n 1 occ, 1)
        Just x  -> (M.insert n (x+1) occ, x+1)

-- sumGrid sums up all values of unmarked case.
sumGrid :: Board -> Int
sumGrid board = M.foldlWithKey' f 0 (grid board)
  where
    unmarked = unMarked board
    f acc key pair
      | pair `S.member` unmarked = acc + key
      | otherwise                = acc

-- Parsing stuff
getDatas :: String -> IO ([Int], [Board])
getDatas filename = do
  (draws, grids) <- parseDatas <$> BC.readFile filename
  let boards = map makeBoard grids
  pure (draws, boards)

-- To map numbers to their coordinates, we assume there are
-- no repetitions of a number inside a grid.
-- This is the case. We can check this, the grid's size of
-- each board should be equal to 25.
makeBoard :: Grid -> Board
makeBoard g =
  Board {rows = M.empty
        ,cols = M.empty
        ,grid = M.fromList g
        ,unMarked = S.fromList (map snd g)
        ,hasWon = False
        }

parseDatas :: ByteString -> ([Int], [Grid])
parseDatas str =
  either error
         id
         (parseOnly parseGame str)

-- parseGame :: Parser ([Int], [Grid])
-- parseGame = do
--   numbers <- decimal `sepBy1'` char ','
--   void (string "\n\n")
--   grids <- parseGrid `sepBy1'` string "\n\n"
--   pure (numbers, grids)

-- Alternative version using Applicative, just for fun!
parseGame :: Parser ([Int], [Grid])
parseGame =
  liftA2 (,)
         (decimal `sepBy1'` char ',' <* string "\n\n")
         (parseGrid `sepBy1'` string "\n\n")

parseGrid :: Parser Grid
parseGrid = makeGrid <$> parseRow `sepBy1'` char '\n'

parseRow :: Parser [Int]
parseRow =
  optional (char ' ')
  *> decimal `sepBy1'` many1 (char ' ')

-- Indices begin at one, not zero. As well, that doesn't matter
-- since we don't use this information.
makeGrid :: [[Int]] -> Grid
makeGrid lss = [(v, (x, y))
                |(y, xs) <- zip  [1..] lss
                ,(x, v) <- zip [1..] xs
                ]
