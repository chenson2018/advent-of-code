import Control.Applicative
import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import Data.Maybe

-- remove duplicates, from SO
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- for any type of 2d lists, create a map of coordinates
grid :: [[a]] -> M.Map (Int, Int) a
grid = M.fromList . concat . zipWith zip [[(x, y) | x <- [0 ..]] | y <- [0 ..]]

-- get key/values to N/S/E/W of a key
surroundingWithKey :: M.Map (Int, Int) Int -> (Int, Int) -> [((Int, Int), Int)]
surroundingWithKey m key = ret
  where
    directions = [(id, (+ 1)), ((+ 1), id), (subtract 1, id), (id, subtract 1)]
    keys = map (($ key) . uncurry bimap) directions
    key_vals = zip keys $ map (`M.lookup` m) keys
    f (_, Nothing) = Nothing
    f (a, Just b) = Just (a, b)
    ret = mapMaybe f key_vals

-- get values to N/S/E/W of a key
surrounding :: M.Map (Int, Int) Int -> (Int, Int) -> [Int]
surrounding m key = map snd $ surroundingWithKey m key

checkLow :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> Bool
checkLow m key val = all (val <) $ surrounding m key

p1 :: M.Map (Int, Int) Int -> Int
p1 m = sum $ (+ 1) <$> M.filterWithKey (checkLow m) m

iter :: M.Map (Int, Int) Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
iter m = concatMap (step m)
  where
    step m (key, value) = filter (((&&) <$> (> value) <*> (/= 9)) . snd) $ surroundingWithKey m key

basins :: M.Map (Int, Int) Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
basins m xs = rmdups $ concat $ takeWhile (/= []) $ iterate (iter m) xs

p2 :: M.Map (Int, Int) Int -> Int
p2 m = product $ take 3 $ sortBy (flip compare) $ map (length . basins m . (: [])) $ M.toList $ M.filterWithKey (checkLow m) m

main =
  do
    input :: M.Map (Int, Int) Int <- grid . (map . map) (read . (: [])) . lines <$> readFile "../input.txt"
    print (p1 input)
    print (p2 input)
