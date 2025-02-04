-- AoC 2021, day 6

{-# LANGUAGE ImportQualifiedPost #-}

{- HLINT ignore "Eta reduce" -}

import Data.Word (Word8)

import Data.List (foldl')
import Data.Functor (($>))
import Control.Monad.Loops (unfoldM)
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

-- we use an IntMap to count occurrences of different timers.
type LanternFishes = IntMap Int

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

main :: IO ()
main = do
  fishes <- getDatas "day6.txt"
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
getDatas :: String -> IO LanternFishes
getDatas filename = parseDatas <$> BC.readFile filename

parseDatas :: ByteString -> LanternFishes
parseDatas str =
  either error
         id
         (scanOnly lanternFishes str)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

eol,comma :: Word8
eol = charToWord8 '\n'
comma = charToWord8 ','

-- The parsing is simple. There is just one line of digits
-- separated by comma.
lanternFishes :: Scanner LanternFishes
lanternFishes = buildMap <$> yaParseLine

-- recursive parser to collect the digits.
parseLine :: Scanner [Int]
parseLine = do
  c <- lookAhead
  case c of
    Nothing            -> pure []
    Just v |v == comma -> word8 v *> parseLine
           |v == eol   -> word8 v $> []
           |otherwise  -> do
              d <- decimal
              ns <- parseLine
              pure (d:ns)

-- Alternative version using Control.Monad.Loops.unFoldM
-- With this one we parse digits separated by any characters.
-- while parseLine par decimal number separated by ',' or
-- '\n'.
-- In the two cases, we don't exactly parse the input string even
-- if they work.
yaParseLine :: Scanner [Int]
yaParseLine = unfoldM parsedDigit
  where
    -- Note: here (digit <* optionalChar) doesn't work.
    -- We need a monad, due to the use of unfoldM.
    parsedDigit = do
      n <- digit
      optionalChar
      pure n

isDigit :: Word8 -> Bool
isDigit d = d  - charToWord8 '0' <= 9

digit :: Scanner (Maybe Int)
digit = do
  c <- lookAhead
  case c of
    Just d |isDigit d -> word8 d
                         $> Just (fromIntegral d - fromEnum '0')
    _                 -> pure Nothing

optionalChar :: Scanner ()
optionalChar = do
  c <- lookAhead
  case c of
    Nothing -> pure ()
    Just v  -> word8 v $> ()

buildMap :: [Int] -> LanternFishes
buildMap ns = foldl' f M.empty ns
  where
    update Nothing = Just 1
    update (Just x) = Just (x+1)

    f acc n = M.alter update n acc
