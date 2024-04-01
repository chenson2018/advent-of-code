import Data.Function (on)
import Data.Maybe (fromJust)
import Parsing

-- parse the trees into a flat list of depths and values
data Flat = Flat
  { depth :: Int,
    val :: Int
  }
  deriving (Show, Eq)

flat :: Int -> Parser [Flat]
flat depth =
  do
    val <- integer
    return [Flat depth val]
    <|> do
      symbol "["
      l <- flat (depth + 1)
      symbol ","
      r <- flat (depth + 1)
      symbol "]"
      return $ l ++ r

parseFlat :: String -> Maybe ([Flat], String)
parseFlat = parse $ flat (-1)

-- splitting calculation
divRound :: Int -> (Int, Int)
divRound i
  | even i = (d, d)
  | odd i = (d, d + 1)
  where
    d = i `div` 2

-- single step of a reduction
-- note the tricky order of pattern matching

step :: [Flat] -> [Flat]
-- case for far right explosion
step
  ( Flat 4 val_el
      : Flat 4 val_er
      : Flat depth_r val_r
      : tl
    ) =
    Flat 3 0 : Flat depth_r (val_er + val_r) : tl
-- case for middle explosion
step
  ( Flat depth_l val_l
      : Flat 4 val_el
      : Flat 4 val_er
      : Flat depth_r val_r
      : tl
    ) =
    Flat depth_l (val_el + val_l) : Flat 3 0 : Flat depth_r (val_er + val_r) : tl
-- case for far left explosion
step
  ( Flat depth_l val_l
      : Flat 4 val_el
      : Flat 4 val_er
      : tl
    ) =
    Flat depth_l (val_el + val_l) : Flat 3 0 : tl
-- case for split or continue
step (x@(Flat depth val) : xs)
  | val >= 10 = Flat (depth + 1) l : Flat (depth + 1) r : xs
  | otherwise = x : step xs
  where
    (l, r) = divRound val
step [] = []

-- generic iterate that stops on consecutive repeated value
iterateUntilRepeat :: (Eq a) => (a -> a) -> a -> [a]
iterateUntilRepeat f init
  | init == next = [init]
  | otherwise = init : iterateUntilRepeat f next
  where
    next = f init

-- reduction steps until stable
reduction :: [Flat] -> [Flat]
reduction = last . iterateUntilRepeat step

-- add and reduce two values
sumReduce :: [Flat] -> [Flat] -> [Flat]
sumReduce a b = reduction $ ((++) `on` inc_depth) a b
  where
    inc_depth = map (\x@Flat {depth} -> x {depth = depth + 1})

-- this looks like State?
-- assumes properly constructed [Flat]
-- inspired by `unflatten` at: https://www.reddit.com/r/haskell/comments/rizwa7/comment/hp14g7i/
magnitude :: [Flat] -> Int
magnitude xs = ret
  where
    (ret, []) = aux (-1) xs
    aux :: Int -> [Flat] -> (Int, [Flat])
    aux depth xs@((Flat d v) : tl)
      | depth == d = (v, tl)
      | otherwise = (3 * l + 2 * r, tl'')
      where
        (l, tl') = aux (depth + 1) xs
        (r, tl'') = aux (depth + 1) tl'

main =
  do
    input@(hd : tl) <- map fst . fromJust . mapM parseFlat . lines <$> readFile "../input.txt"
    print $ magnitude $ foldl sumReduce hd tl
    print $ maximum $ map magnitude $ sumReduce <$> input <*> input
