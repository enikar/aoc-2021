-- AoC 2021, day 4, part1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import Data.Maybe
  (isNothing
  ,isJust
  ,catMaybes
  )
import Data.Array
  (Array
  ,elems
  ,assocs
  ,array
  )
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

type Grid = Array (Int, Int) (Maybe Int)
type Bingo = ([Int], [Grid])

main :: IO ()
main = do
  bingo <- getDatas "day4.txt"
  printSolution "Part1" (part1 bingo)
  --printSolution "Part2" (part2 bingo)

-- part1 lays each draw number on each grid if relevant then checks each
-- grid if there is one with a row or column fully filled we compute the
-- sum of all unchecked box an multiply this with the last draw number.
part1 :: Bingo -> Int
part1 (draws, gs) = n * s
  where
    s = sum (catMaybes (elems g))
    -- We use foldr to short circuit, but we need to reverse draws
    -- to evaluate each draw in the order they appear.
    (n,g) = case foldr f (Nothing,gs) (reverse draws) of
              (Just n', [g']) -> (n', g')
              _               -> error "Error: Part1"

    f d (p,grids)
      |isJust p   = (p, grids)
      |otherwise =
       let grids' = next d grids
       in case check grids' of
         Just grid -> (Just d, [grid])
         Nothing   -> (Nothing, grids')

-- part2 determines the score of the last grid that wins instead of
-- the first as in part1. I'm stuck on this.
part2 :: Bingo -> Int
part2 = undefined

check :: [Grid] -> Maybe Grid
check grids = foldr f Nothing grids
  where
    f _ (Just grid) = Just grid
    f grid Nothing
      | checkRows grid' || checkColumns grid' = Just grid
      | otherwise                             = Nothing
        where
          grid' = assocs grid

checkRows :: [((Int, Int), Maybe Int)] -> Bool
checkRows grid = foldr f False [1..5]
  where
    f _ True = True
    f r False = all isNothing [n | ((_, y), n) <- grid, y == r]

checkColumns :: [((Int, Int), Maybe Int)] -> Bool
checkColumns grid = foldr f False [1..5]
  where
    f _ True = True
    f c False = all isNothing [n |((x, _), n) <- grid, x == c]

next :: Int -> [Grid] -> [Grid]
next n grids = map (fillGrid n) grids

fillGrid :: Int -> Grid -> Grid
fillGrid n grid = fmap f grid
  where
    f x | x == Just n = Nothing
        | otherwise   = x

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

-- Parsing stuff
getDatas :: String -> IO Bingo
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> Bingo
parseDatas str =
  either error
         id
         (parseOnly parseBingo str)

-- parseBingo :: Parser Bingo
-- parseBingo = do
--   numbers <- decimal `sepBy1'` char ','
--   void (string "\n\n")
--   grids <- parseGrid `sepBy1'` string "\n\n"
--   pure (numbers, grids)

-- Using Applicative, just for fun!
parseBingo :: Parser Bingo
parseBingo =
  liftA2 (,)
         (decimal `sepBy1'` char ',' <* string "\n\n")
         (parseGrid `sepBy1'` string "\n\n")

parseGrid :: Parser Grid
parseGrid = buildGrid <$> parseRow `sepBy1'` char '\n'

parseRow :: Parser [Int]
parseRow =
  optional (char ' ')
  *> decimal `sepBy1'` many1 (char ' ')

buildGrid :: [[Int]] -> Grid
buildGrid lss = array ((1,1), (xsup, ysup)) as
  where
    ysup = length lss
    xsup = length (head lss)
    as = [((x, y), Just v)
         |(y, xs) <- zip  [1..] lss
         ,(x, v) <- zip [1..] xs
         ]
