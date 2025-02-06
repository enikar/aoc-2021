-- AoC 2021, day 6, version for hugs just for fun.
-- Usage: runhugs -98 hday6-hugs.hs

import Data.Char
  (isDigit
  ,digitToInt
  )
-- Since Int means Int32 in hugs, we import explicitly Int64.
import Data.Int (Int64)
import Data.List (foldl')
-- we need to import Applicative explicitly
import Control.Applicative
  (Applicative(..)
  ,(<$>)
  ,(<*)
  )
import Control.Monad
  (unless
  ,ap
  )
-- The definition of sum in hugs Prelude is for List.
-- As well, in hugs,  many names in Data.Foldable clash with names
-- in its Prelude, so it's better to import the module qualified.
import qualified Data.Foldable as Foldable
-- Data.IntMap.Strict doesn't exist in hugs.
import Data.IntMap(IntMap)
import qualified Data.IntMap as M

import Text.ParserCombinators.ReadP
  (ReadP
  ,sepBy1
  ,char
  ,satisfy
  ,readP_to_S
  ,pfail
  ,look
  ,optional
  )

-- we use an IntMap to count occurrences of different timers.
type LanternFishes = IntMap Int64

-- Monoid class exists but it doesn't define (<>). So we use (++) instead.
printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part ++ ": " ++ show sol)

main :: IO ()
main = do
  fishes <- getDatas "day6.txt"
  printSolution "Part1" (partx 80 fishes)
  printSolution "Part2" (partx 256 fishes)

-- Since InMaps are Foldable, we can use Foldable.sum to
-- add all values to count lanternfishes.
partx :: Int -> IntMap Int64 -> Int64
partx n fishes = Foldable.sum (times n next fishes)

-- In hugs there is no foldlWithKey. Instead there is foldWithKey
-- which is a foldrWithKey
next :: LanternFishes -> LanternFishes
next fishes = M.foldWithKey f M.empty fishes
  where
    update n Nothing  = Just n
    update n (Just x) = Just (n+x)

    f timer n acc
      |timer == 0 = M.insert 8 n (M.alter (update n) 6 acc)
      |timer == 7 = M.alter (update n) 6 acc
      |otherwise  = M.insert (timer - 1) n acc

-- borrow from: https://github.com/glguy/advent/blob/main/common/src/Advent/Prelude.hs
times :: Int -> (a -> a) -> a -> a
times n f x
  |n <= 0    = x
  |otherwise = times (n-1) f $! f x

-- Parsing stuff
getDatas :: String -> IO LanternFishes
getDatas filename = parseDatas <$> (readFile filename)

-- The parsing is simple. There is just one line of digits
-- separated by comma.
parseDatas :: String -> LanternFishes
parseDatas str = case readP_to_S lanternFishes str of
                   [(x, "")] -> x
                   _         -> error "Can't parse."

-- Hugs doesn't provide an Applicative instance for ReadP.
-- So we write a simple one.
instance Applicative ReadP where
  pure = return
  (<*>) = ap

lanternFishes :: ReadP LanternFishes
lanternFishes =
  buildMap <$> (parseDigits <* optional (char '\n') <* eof)

parseDigits :: ReadP [Int]
parseDigits =
  map digitToInt <$> (satisfy isDigit `sepBy1` char ',')


-- eof desn't exist in the ReadP bundled with hugs
eof :: ReadP ()
eof = do
  s <- look
  unless (null s) pfail

buildMap :: [Int] -> LanternFishes
buildMap ns = foldl' f M.empty ns
  where
    update Nothing = Just 1
    update (Just x) = Just (x+1)

    f acc n = M.alter update n acc
