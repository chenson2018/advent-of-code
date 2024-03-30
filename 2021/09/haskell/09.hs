import Control.Applicative
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe

grid :: [[a]] -> M.Map (Int, Int) a
grid = M.fromList . concat . zipWith zip [[(x, y) | x <- [0 ..]] | y <- [0 ..]]

surrounding :: M.Map (Int, Int) Int -> (Int, Int) -> [Int]
surrounding m key = vals
  where
    directions = [(id, (+ 1)), ((+ 1), id), (subtract 1, id), (id, subtract 1)]
    keys = map (($ key) . uncurry bimap) directions
    vals = mapMaybe (`M.lookup` m) keys

checkLow :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> Bool
checkLow m key val = all (val <) $ surrounding m key

p1 :: M.Map (Int, Int) Int -> Int
p1 m = sum $ (+ 1) <$> M.filterWithKey (checkLow m) m

main =
  do
    input :: M.Map (Int, Int) Int <- grid . (map . map) (read . (: [])) . lines <$> readFile "../test.txt"
    input :: M.Map (Int, Int) Int <- grid . (map . map) (read . (: [])) . lines <$> readFile "../input.txt"
    print (p1 input)
