import Data.List

subsum :: Int -> [Int] -> [[Int]]
subsum n = filter ((== n) . sum) . subsequences

-- a fold that counts the number of lists with minimum length
countMinLength :: [[a]] -> (Maybe Int, Maybe Int)
countMinLength = foldr f (Nothing, Nothing)
  where
    j (a, b) = (Just a, Just b)
    f xs (Just len, Just count)
      | length xs == len = j (len, count + 1)
      | length xs < len = j (length xs, 1)
      | otherwise = j (len, count)
    f xs (Nothing, Nothing) = j (length xs, 1)

main = do
  input <- map read . lines <$> readFile "../input.txt"
  let combos = subsum 150 input
  print (length combos)
  print (snd $ countMinLength combos)
