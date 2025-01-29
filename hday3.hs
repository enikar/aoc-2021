-- AoC 2021, day 3

-- Naive solution. It needs improvements.

{- HLINT ignore "Eta reduce" -}

import System.IO (readFile')
import Data.List (foldl')
import Data.Bool (bool)
import Data.Bits
  (complement
  ,(.&.)
  ,shiftL
  )

import Data.Array.Unboxed
  (UArray
  ,array
  ,bounds
  ,assocs
  )

-- We use Bool to represent a binary digit
type Report = UArray (Int, Int) Bool

printSolution :: Show a => String -> a -> IO ()
printSolution part sol = putStrLn (part <> ": " <> show sol)

getDatas :: String -> IO Report
getDatas filename = parseDatas <$> readFile' filename

parseDatas :: String -> Report
parseDatas str = array ((1,1), (xsup, ysup)) ls
  where
    strs = lines str
    ysup = length strs
    xsup = length (head strs)
    ls = [((x, y), b)
         |(y, s) <- zip [1..] strs
         ,(x, b) <- zip [1..] (bits s)
         ]
    bits :: String -> [Bool]
    bits s = map g s
      where
        g '0' = False
        g '1' = True
        g _   = error "Error: bad input"

main :: IO ()
main = do
  report <- getDatas "day3.txt"
  printSolution "Part1" (part1 report)
  printSolution "Part2" (part2 report)


part1 :: Report -> Int
part1 report = getResult (foldr f [] [1..xsup])
  where
    ((_, _), (xsup,_)) = bounds report
    getResult gs = gamma * epsilon
      where
        gamma = fromBits gs
        epsilon = complement gamma .&. (1 `shiftL` xsup - 1)

    f x acc
      | zero > one = 0 : acc
      | otherwise  = 1 : acc
        where
          (one, zero) = countZeroAndOne (getX x report)


-- returns values of the column x
getX :: Int -> Report -> [Bool]
getX x report = [b
                |((x', _), b) <- assocs report
                ,x' == x
                ]

-- returns values of the row y
getY :: Int -> Report -> [Bool]
getY y report = [b
                |((_, y'), b) <- assocs report
                ,y' == y
                ]
-- returns the pair of (Ones, Zeros) in bits
countZeroAndOne :: [Bool] -> (Int, Int)
countZeroAndOne bits = foldl' h (0, 0) bits
  where
    h (one, zero) b
      |b         = (one+1, zero)
      |otherwise = (one, zero+1)

-- Computes an Int from its binary representation
-- Examples: fromBits [1, 0, 1] == 5
--           fromBits [1, 0] == 2
fromBits :: [Int] -> Int
fromBits xs = foldl' g 0 xs
  where
    g acc x = 2 * acc + x

-- part2 is quite slow.
part2 :: Report -> Int
part2 report = oxygen * dioxy
  where
    fromBits' = fromBits . map (bool 0 1)

    oxygen = fromBits' (getY 1 (oxygenRating report))
    dioxy  = fromBits' (getY 1 (dioxydRating report))

oxygenRating :: Report -> Report
oxygenRating = rating (>) -- we select the most frequent bits

dioxydRating :: Report -> Report
dioxydRating = rating (<=) -- we select the least frequent bits

-- rating reduces the report by selecting corresponding rows to either
-- most common bits in a column or the least frequent bit in a column.
-- That depends on `cmp`.
-- We end when the report is an UArray of only one row.
-- We use a foldr to whort-circuit.
rating :: (Int -> Int -> Bool) -> Report -> Report
rating cmp report = foldr f report (reverse [1..xsup])
  where
    ((_,_), (xsup, _)) = bounds report
    f x acc
      |ysup == 1      = acc
      |zero `cmp` one = select False x acc
      |otherwise      = select True x acc
       where
         ((_, _), (_, ysup)) = bounds acc
         (one, zero) = countZeroAndOne (getX x acc)


-- selects all "lines" (y) where (report ! (x,y)) == b)
-- This is somewhat tricky
select :: Bool -> Int -> Report -> Report
select b x report = array ((1, 1), (xsup, ysup)) ls'
  where
    ((_,_), (xsup, _)) = bounds report
    -- first, we collect all the y where the (report ! (x,y)) == b
    ys =  [y
          |((x',y), b') <- assocs report
          ,x' == x
          ,b' == b
          ]
    -- next, we build the list of all row
    ls = map (`getY` report) ys
    ysup = length ls
    -- last, we compute the assocs to build the new report
    ls' = [((x',y), b')
          |(y, xs) <- zip [1..] ls
          ,(x', b') <- zip [1..] xs
          ]
