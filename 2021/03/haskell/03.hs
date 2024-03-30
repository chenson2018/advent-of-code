import Data.Function
import Data.List

epsilonDelta :: [Int] -> (Int, Int)
epsilonDelta xs = (e, d)
  where
    [e, d] = (map head . sortOn length . group . sort) xs

bin :: [Int] -> Int
bin = aux 1 . reverse
  where
    aux _ [] = 0
    aux n (x : xs) = n * x + aux (2 * n) xs

p1 :: [[Int]] -> Int
p1 xs = ((*) `on` (bin . flip map ed)) fst snd
  where
    ed = map epsilonDelta $ transpose xs

nFilt :: Int -> Int -> [[Int]] -> [[Int]]
nFilt idx val = filter ((== val) . (!! idx))

tiebreak :: Int -> [(Int, Int)] -> [Int]
tiebreak tie = map getProj
  where
    getProj (x, y)
      | x == y = tie
      | tie == 0 = x
      | tie == 1 = y

rating :: Int -> [[Int]] -> Int -> Int
rating _ [x] _ = bin x
rating n xs tie = rating (n + 1) next tie
  where
    tie_ed = tiebreak tie (map epsilonDelta $ transpose xs) !! n
    next = nFilt n tie_ed xs

p2 :: [[Int]] -> Int
p2 xs = ((*) `on` rating 0 xs) 0 1

main =
  do
    input :: [[Int]] <- (map . map) (read . (: [])) . lines <$> readFile "../input.txt"
    print (p1 input)
    print (p2 input)
