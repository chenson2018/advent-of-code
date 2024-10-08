import Data.List

swap = (`mod` 2) . (+ 1)

iter a = a ++ 0 : (map swap . reverse) a

checksum xs
  | length xs `mod` 2 == 1 = xs
  | otherwise = checksum $ aux xs
  where
    aux (a : b : xs) = (fromEnum (a == b)) : aux xs
    aux [] = []

p1 init len = do
  filled <- find ((>= len) . length) (iterate iter init)
  return $ checksum $ take len $ filled

main = do
  let input = map (read . (: [])) "10001110011110000"
  print $ p1 input 272
  print $ p1 input 35651584
