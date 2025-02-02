-- AoC 2021, day 6

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import Data.Word (Word8)

import Data.List (foldl')
import Data.Functor (($>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Scanner
  (Scanner
  ,scanOnly
  ,lookAhead
  ,word8
  ,decimal
  )

type LanternFishes = IntMap Int


printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

main :: IO ()
main = do
  fishes <- getDatas "day6.txt"
  printSolution "Part1" (partx 80 fishes)
  printSolution "Part2" (partx 256 fishes)

partx :: Int -> IntMap Int -> Int
partx n fishes = M.foldl' (+) 0 (times n next fishes)

next :: LanternFishes -> LanternFishes
next fishes = M.foldlWithKey' f M.empty fishes
  where
    update n Nothing = Just n
    update n (Just x) = Just (n+x)

    f acc timer n
      |timer == 0 = M.insert 8 n (M.alter (update n) 6 acc)
      |timer == 7 = M.alter (update n) 6 acc
      |otherwise = M.insert (timer - 1) n acc

-- borrow from: https://github.com/glguy/advent/blob/main/common/src/Advent/Prelude.hs
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x

-- Parsing stuff
getDatas :: String -> IO LanternFishes
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> LanternFishes
parseDatas str =
  either error
         id
         (scanOnly parseMap str)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

eol,comma :: Word8
eol = charToWord8 '\n'
comma = charToWord8 ','

parseMap :: Scanner LanternFishes
parseMap = buildMap <$> parseLine

parseLine :: Scanner [Int]
parseLine = do
  c <- lookAhead
  case c of
    Nothing             -> pure []
    Just v | v == comma -> word8 v *> parseLine
           | v == eol   -> word8 v $> []
           | otherwise  -> do
               d <- decimal
               ns <- parseLine
               pure (d:ns)

buildMap :: [Int] -> LanternFishes
buildMap ns = foldl' f M.empty ns
  where
    update Nothing = Just 1
    update (Just x) = Just (x+1)

    f acc n = M.alter update n acc
