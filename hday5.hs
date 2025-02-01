-- AoC 2021, Day 5

-- it is quite slow, it needs improvements.
-- To solve the puzlle, we need to read segments from day5.txt.
-- Each segment is described by the coordinates of two points (in 2D).
-- For part1 we need to detect points where horizontal and vertical
-- segments overlap.
-- For part2 we also consider diagonal segments.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Attoparsec.ByteString.Char8
  (Parser
  ,parseOnly
  ,decimal
  ,string
  ,char
  ,sepBy1'
  )

type Coord = (Int, Int)
type Segment = (Coord, Coord)
type Line = [Coord]

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

main :: IO ()
main = do
  segs <- getDatas "day5.txt"
  let (hs, vs, ds) = partitionSegments segs
      hsVs =  map horizontalSeg hs ++ map verticalSeg vs
      grid  = partx hsVs M.empty
  printSolution "Part1" (countOverlap grid)
  printSolution "Part2" (countOverlap (partx (map diagonalSeg ds) grid))

partx :: [Line] -> Map Coord Int -> Map Coord Int
partx segs grid = foldl' f grid segs
  where
    f acc ss = foldl' g acc ss

    g grid' s = M.alter updateGrid s grid'

    updateGrid Nothing = Just 1
    updateGrid (Just v) = Just (v+1)

countOverlap :: M.Map Coord Int -> Int
countOverlap = M.foldl' h 0
  where
    h acc x |x > 1     = acc+1
            |otherwise = acc

horizontalSeg :: Segment -> Line
horizontalSeg ((x1, y), (x2, _)) = [(x,y) | x <- xs]
  where
    xs | x1 < x2   = [x1..x2]
       | otherwise = [x2..x1]

verticalSeg :: Segment -> Line
verticalSeg ((x, y1), (_, y2)) =  [(x,y) | y <- ys]
  where
    ys | y1 < y2   = [y1..y2]
       | otherwise = [y2..y1]

diagonalSeg :: Segment -> Line
diagonalSeg ((x1, y1), (x2, y2)) = zip xs ys
  where
    xs | x1 < x2   = [x1..x2]
       | otherwise = [x1,x1-1..x2]

    ys | y1 < y2   = [y1..y2]
       | otherwise = [y1, y1-1..y2]

-- Alternative version all in one
-- segToLine :: Segment -> Line
-- segToLine ((x1, y1), (x2, y2)) = zip xs ys
--   where
--     xs | x1 < x2   = [x1..x2]
--        | x1 == x2  = repeat x1
--        | otherwise = [x1,x1-1..x2]

--     ys | y1 < y2   = [y1..y2]
--        | y1 == y2  = repeat y1
--        | otherwise = [y1, y1-1..y2]


partitionSegments :: [Segment] -> ([Segment], [Segment], [Segment])
partitionSegments segs = foldr f ([], [], []) segs
  where
    f seg@((x1, y1), (x2, y2)) (horizontals, verticals, diagonals)
      | x1 == x2  = (horizontals, seg : verticals, diagonals)
      | y1 == y2  = (seg : horizontals, verticals, diagonals)
      | otherwise = (horizontals, verticals, seg : diagonals)

-- Parsing stuff
getDatas :: String -> IO [Segment]
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> [Segment]
parseDatas str =
  either error
         id
         (parseOnly parseSegments str)

parseSegments :: Parser [Segment]
parseSegments = parseSegment `sepBy1'` string "\n"

parseSegment :: Parser Segment
parseSegment =
  liftA2 (,)
         (parseCoordinates <* string " -> ")
         parseCoordinates

parseCoordinates :: Parser Coord
parseCoordinates =
  liftA2 (,)
         (decimal <* char ',')
         decimal
