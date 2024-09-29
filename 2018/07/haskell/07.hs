import qualified Data.Map as M
import Parsing
import Data.List

extractFirst f xs = aux f xs []
  where
    aux :: (a -> Bool) -> [a] -> [a] -> Maybe (a, [a])
    aux _ [] _ = Nothing
    aux f (x:r) l
      | f x = Just (x, reverse l ++ r)
      | otherwise = aux f r (x:l)

line = 
  do
  symbol "Step"
  l <- upper
  symbol "must be finished before step"
  r <- upper
  return (l, r)

buildMap xs = aux (sortBy (flip compare) xs) M.empty 
  where
   aux [] m = m
   aux ((r,l):xs) m = aux xs new_m
     where 
       up Nothing = [r]
       up (Just xs) = (r:xs)
       new_m = M.alter (Just . up) l m

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

addStart m = M.assocs $ foldl (\m' k -> M.insert k "@" m') m ((rmdups $ concat $ M.elems m) \\ (M.keys m))

iter open [] = open
iter open remain = 
  case extractFirst (all (`elem` open). snd) remain of
    Just (no, remain') -> iter (fst no:open) remain'

main = do
  input <- parseListJust line . lines <$> readFile "../test.txt"
  input <- parseListJust line . lines <$> readFile "../input.txt"
  let m = addStart $ buildMap input
  print $ tail $ reverse $ iter "@" m
