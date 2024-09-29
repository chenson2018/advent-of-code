{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Parsing

extractFirst f xs = aux f xs []
  where
    aux :: (a -> Bool) -> [a] -> [a] -> Maybe (a, [a])
    aux _ [] _ = Nothing
    aux f (x : r) l
      | f x = Just (x, reverse l ++ r)
      | otherwise = aux f r (x : l)

line =
  do
    symbol "Step"
    l <- upper
    symbol "must be finished before step"
    r <- upper
    return (l, r)

mapAndStart [] m ls rs = (m, ls S.\\ rs)
mapAndStart ((l, r) : xs) m ls rs = mapAndStart xs new_m (S.insert l ls) (S.insert r rs)
  where
    up Nothing = [l]
    up (Just xs) = (l : xs)
    new_m = M.alter (Just . up) r m

buildMap xs = M.assocs $ uncurry (foldl (\m' k -> M.insert k "@" m')) $ mapAndStart (sortBy (flip compare) xs) M.empty S.empty S.empty

iter open [] = open
iter open remain =
  case extractFirst (all (`elem` open) . snd) remain of
    Just (no, remain') -> iter (fst no : open) remain'

main = do
  -- input <- parseListJust line . lines <$> readFile "../test.txt"
  input <- parseListJust line . lines <$> readFile "../input.txt"
  let m = buildMap input
  print $ tail $ reverse $ iter "@" m
