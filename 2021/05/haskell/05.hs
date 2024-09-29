import Data.Function
import Data.List (group, intersect, intersectBy, sort, subsequences, tails)
import Data.Maybe
import Parsing

data Line = Line
  { x1 :: Int,
    y1 :: Int,
    x2 :: Int,
    y2 :: Int
  }
  deriving (Show)

line :: Parser Line
line =
  do
    x1 <- integer
    symbol ","
    y1 <- integer
    symbol "->"
    x2 <- integer
    symbol ","
    y2 <- integer
    return Line {x1, y1, x2, y2}

isVertical :: Line -> Bool
isVertical Line {x1, x2} = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal Line {y1, y2} = y1 == y2

slope :: Line -> Maybe Rational
slope Line {x1, y1, x2, y2}
  | x1 == x2 = Nothing
  | otherwise = Just $ ((-) `on` toRational) y2 y1 / ((-) `on` toRational) x2 x1

pointsHV line@(Line {x1, y1, x2, y2})
  | isHorizontal line = zip [min x1 x2 .. max x1 x2] (repeat y1)
  | isVertical line = zip (repeat x1) [min y1 y2 .. max y1 y2]
  | otherwise = []

main = do
  input <- map fst . fromJust . mapM (parse line) . lines <$> readFile "../test.txt"
  input <- map fst . fromJust . mapM (parse line) . lines <$> readFile "../input.txt"
  print $ length $ filter ((> 1) . length) $ group $ sort $ concat $ map pointsHV $ filter (\x -> isVertical x || isHorizontal x) input
