import Data.List
import Data.Maybe
import Parsing

-- surprisingly tricky to make this work for variable lengths
aux :: String -> String -> (String, String) -> [String]
aux _ [] _ = []
aux prev front (old, new)
  | old `isPrefixOf` front = (reverse prev ++ new ++ pastPrefix) : continue
  | otherwise = continue
  where
    pastPrefix = drop (length old) front
    continue = aux (head front : prev) (drop 1 front) (old, new)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

replace :: String -> (String, String) -> [String]
replace = aux ""

chemMap :: Parser (String, String)
chemMap = do
  old <- many letter
  symbol "=>"
  new <- many letter
  return (old, new)

iter :: [(String, String)] -> [String] -> [String]
iter mappings xs = concat $ replace <$> xs <*> mappings

main = do
  (input : _ : mappings_raw) <- reverse . lines <$> readFile "../input.txt"
  let mappings = map fst $ fromJust $ mapM (parse chemMap) mappings_raw
  print (length $ rmdups $ iter mappings [input])
