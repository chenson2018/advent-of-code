import Data.Bifunctor (Bifunctor (first))
import Data.List
import Parsing

splitInput = aux []
  where
    aux ret [] = [ret]
    aux ret ("" : xs) = ret : aux [] xs
    aux ret (x : xs) = aux (x : ret) xs

cond :: Parser (String, Int -> Bool)
cond =
  do
    name <- many identifier
    symbol ":"
    a <- integer
    symbol "-"
    b <- integer
    symbol "or"
    c <- integer
    symbol "-"
    d <- integer
    return (concat name, \x -> (a <= x && x <= b) || (c <= x && x <= d))

noMatch :: [(String, Int -> Bool)] -> Int -> Bool
noMatch conditions x = not $ any (($ x) . snd) conditions

p2 :: [(String, Int -> Bool)] -> [[Int]] -> [Int] -> Int
p2 conditions noInvalid ticket = ret
  where
    -- given a column, what names it could possibly be
    couldBe conditions col = map fst $ filter ((`all` col) . snd) conditions
    -- possibilities for each column
    combos = map (couldBe conditions) (transpose noInvalid)
    -- noticing that the length of these is 1, 2, ..., eliminate one at each step
    ci = sortOn (length . fst) $ zip combos [0 ..]
    iter [] = []
    iter (x@([name], n) : tl) = (name, n) : iter (map (first (filter (/= name))) tl)
    -- get the requested conditions for final answer
    ret = product $ map ((ticket !!) . snd) . filter (isPrefixOf "depart" . fst) $ iter ci

main =
  do
    [conditions_raw, [ticket_raw, _], nearby_raw] <- splitInput . lines <$> readFile "../input.txt"
    let conditions = parseListJust cond conditions_raw
    let [ticket] = parseListJust (list "," integer) [ticket_raw]
    let nearby = parseListJust (list "," integer) $ init nearby_raw
    print $ sum $ filter (noMatch conditions) $ concat nearby
    let noInvalid = filter (not . any (noMatch conditions)) nearby
    print $ p2 conditions noInvalid ticket
