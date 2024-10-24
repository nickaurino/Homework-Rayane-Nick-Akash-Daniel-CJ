module Exercises
    ( change
    , firstThenApply
    , powers
    , meaningfulLineCount
    , Shape(Sphere, Box)
    , surfaceArea
    , volume
      -- put the proper exports here
    ) where

import qualified Data.Map as Map
import Data.Text (pack, unpack, replace)
import Data.Text.Internal
import Data.List(isPrefixOf, find)
import Data.Char(isSpace)
import System.IO
import Control.Exception (bracket)

import Text.Printf(printf)

change :: Integer -> Either String (Map.Map Integer Integer)
change amount
    | amount < 0 = Left "amount cannot be negative"
    | otherwise = Right $ changeHelper [25, 10, 5, 1] amount Map.empty
        where
          changeHelper [] remaining counts = counts
          changeHelper (d:ds) remaining counts =
            changeHelper ds newRemaining newCounts
              where
                (count, newRemaining) = remaining `divMod` d
                newCounts = Map.insert d count counts

-- Write your first then apply function here
firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs p f = f <$> find p xs

-- Write your infinite powers generator here
powers :: Integral a => a -> [a]
powers base = map (base ^) [0..]

-- Write your line count function here
meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount path = do
    contents <- readFile path
    return $ length $ filter meaningfulLine (lines contents)

meaningfulLine :: String -> Bool
meaningfulLine line =
    let trimmedLine = dropWhile isSpace line
    in not (null trimmedLine)
       && head trimmedLine /= '#'

-- Write your shape data type here
data Shape
    = Box Double Double Double
    | Sphere Double
    deriving (Show, Eq)

surfaceArea :: Shape -> Double
surfaceArea (Box w h d) = 2 * (w * h + w * d + h * d)
surfaceArea (Sphere r)  = 4 * pi * r^2

volume :: Shape -> Double
volume (Box w h d) = w * h * d
volume (Sphere r)  = (4 / 3) * pi * r^3

-- Write your binary search tree algebraic type here
