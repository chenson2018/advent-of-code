import Data.List (find, findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Parsing

line :: Parser (Int, Int)
line =
  do
    depth <- integer
    symbol ":"
    range <- integer
    return (depth, range)

severity :: Int -> Int -> Int
severity depth range
  | 0 == depth `mod` (2 * range - 2) = depth * range
  | otherwise = 0

isCaught :: Int -> Int -> Int -> Bool
isCaught wait depth range = 0 == (depth + wait) `mod` (2 * range - 2)

main =
  do
    input <- M.fromList . parseListJust line . lines <$> readFile "../input.txt"
    print $ sum $ M.mapWithKey severity input
    print $ findIndex id $ map (\w -> not $ or $ M.elems $ M.mapWithKey (isCaught w) input) [0 ..]
