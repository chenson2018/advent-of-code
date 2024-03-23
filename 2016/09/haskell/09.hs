import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

data Part = Part1 | Part2

count :: Part -> String -> Int
count part ('(' : xs) = recurse part * rep + count part rest
  where
    (n_raw, r1) = span (/= 'x') xs
    (rep_raw, r2) = span (/= ')') (tail r1)
    n :: Int = read n_raw
    rep :: Int = read rep_raw
    (hd, rest) = splitAt n (tail r2)
    recurse Part1 = n
    recurse Part2 = count part hd
count part (_ : xs) = 1 + count part xs
count _ [] = 0

main = do
  input <- trim <$> readFile "../input.txt"
  print (count Part1 input)
  print (count Part2 input)
