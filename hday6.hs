-- AoC 2021, day 6

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

{- HLINT ignore "Eta reduce" -}
import System.IO (readFile')

import Data.Char
  (isDigit
  ,digitToInt
  )
import Data.List (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Control.Monad.IO.Class (liftIO)
import "mtl" Control.Monad.Except
  (ExceptT
  ,liftEither
  ,runExceptT
  )
import Text.ParserCombinators.ReadP
  (ReadP
  ,eof
  ,sepBy1
  ,char
  ,satisfy
  ,optional
  ,readP_to_S
  )
-- we use an IntMap to count occurrences of different timers.
type LanternFishes = IntMap Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

-- main :: IO ()
-- main = do
--   r <- runExceptT (getDatas "day6.txt")
--   case r of
--     Left e -> putStrLn ("Error: " <> e)
--     Right fishes -> do
--       printSolution "Part1" (partx 80 fishes)
--       printSolution "Part2" (partx 256 fishes)
-- Alternative way to write main, nicer in my opinion.
main :: IO ()
main = runExceptT (getDatas "day6.txt") >>= either printError goOn
  where
    printError e = putStrLn ("Error: " <> e)

    goOn fishes = do
      printSolution "Part1" (partx 80 fishes)
      printSolution "Part2" (partx 256 fishes)

-- Since InMaps are Foldable, we can use sum to add
-- all values to count lanternfishes.
partx :: Int -> IntMap Int -> Int
partx n fishes = sum (times n next fishes)

next :: LanternFishes -> LanternFishes
next fishes = M.foldlWithKey' f M.empty fishes
  where
    update n Nothing  = Just n
    update n (Just x) = Just (n+x)

    f acc timer n
      |timer == 0 = M.insert 8 n (M.alter (update n) 6 acc)
      |timer == 7 = M.alter (update n) 6 acc
      |otherwise  = M.insert (timer - 1) n acc

-- borrow from: https://github.com/glguy/advent/blob/main/common/src/Advent/Prelude.hs
times :: Int -> (a -> a) -> a -> a
times n f x
  |n <= 0    = x
  |otherwise = times (n-1) f $! f x

-- Parsing stuff
getDatas :: String -> ExceptT String IO LanternFishes
getDatas filename = do
  str <- liftIO (readFile' filename)
  liftEither (parseDatas str)

-- Another way to write it:
-- getDatas filename =
--   liftIO (readFile' filename)
--   >>= liftEither . parseDatas

-- The parsing is simple. There is just one line of digits
-- separated by comma.
parseDatas :: String -> Either String LanternFishes
parseDatas str = case readP_to_S lanternFishes str of
                   [(x, "")] -> Right x
                   _         -> Left "Can't parse."

lanternFishes :: ReadP LanternFishes
lanternFishes =
  buildMap <$> (parseDigits <* optional (char '\n') <* eof)

parseDigits :: ReadP [Int]
parseDigits =
  map digitToInt <$> (satisfy isDigit `sepBy1`  char ',')

buildMap :: [Int] -> LanternFishes
buildMap ns = foldl' f M.empty ns
  where
    update Nothing = Just 1
    update (Just x) = Just (x+1)

    f acc n = M.alter update n acc
