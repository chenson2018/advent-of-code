import Control.Monad.State.Lazy
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
      l <- flat $ depth + 1
      symbol ","
      r <- flat $ depth + 1
      symbol "]"
      return $ l ++ r

parseFlat :: String -> Maybe ([Flat], String)
parseFlat = parse $ flat 0

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
  ( Flat 5 val_el
      : Flat 5 val_er
      : Flat depth_r val_r
      : tl
    ) =
    Flat 4 0 : Flat depth_r (val_er + val_r) : tl
-- case for middle explosion
step
  ( Flat depth_l val_l
      : Flat 5 val_el
      : Flat 5 val_er
      : Flat depth_r val_r
      : tl
    ) =
    Flat depth_l (val_el + val_l) : Flat 4 0 : Flat depth_r (val_er + val_r) : tl
-- case for far left explosion
step
  ( Flat depth_l val_l
      : Flat 5 val_el
      : Flat 5 val_er
      : tl
    ) =
    Flat depth_l (val_el + val_l) : Flat 4 0 : tl
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
sumReduce a b = reduction $ inc_depth $ a ++ b
  where
    inc_depth = map (\x@Flat {depth} -> x {depth = depth + 1})

-- this generally accumulates a generic State from [Flat] according to its tree structure
-- parameters leaf and node are functions to handle each case
-- inspired by `unflatten` at: https://www.reddit.com/r/haskell/comments/rizwa7/comment/hp14g7i/

{- ORMOLU_DISABLE -}

treeMapAtDepth :: Int -> (Int -> s) -> (s -> s -> s) -> [Flat] -> State (Maybe s) [Flat]
treeMapAtDepth depth leaf node xs@(Flat d v : tl) =
  if depth == d
  then 
    do put $ Just $ leaf v
       return tl
  else 
    let next fs = runState $ treeMapAtDepth (depth + 1) leaf node fs in 
    do s <- get
       let (tl', l) = next xs s
       let (tl'', r) = next tl' s
       put $ node <$> l <*> r
       return tl''
treeMapAtDepth _ _ _ [] = state ([],)

{- ORMOLU_ENABLE -}

treeMap :: (Int -> s) -> (s -> s -> s) -> [Flat] -> Maybe s
treeMap leaf node xs =
  case runState (treeMapAtDepth 0 leaf node xs) Nothing of
    ([], Just s) -> Just s
    _ -> Nothing

magnitude :: [Flat] -> Maybe Int
magnitude = treeMap id (\l r -> 3 * l + 2 * r)

-- Tree representation
-- not used, just an example of the generality of treeMap
data Snailfish
  = Val Int
  | Pair Snailfish Snailfish
  deriving (Show)

toSnailfish :: [Flat] -> Maybe Snailfish
toSnailfish = treeMap Val Pair

-- Here are versions not using State
-- note how the subtlety of the initial value disappears...

treeMapAtDepth' :: Int -> (Int -> s) -> (s -> s -> s) -> [Flat] -> ([Flat], s)
treeMapAtDepth' depth leaf node xs@((Flat d v) : tl)
  | depth == d = (tl, leaf v)
  | otherwise = (tl'', node l r)
  where
    next = treeMapAtDepth' (depth + 1) leaf node
    (tl', l) = next xs
    (tl'', r) = next tl'

treeMap' :: (Int -> s) -> (s -> s -> s) -> [Flat] -> Maybe s
treeMap' leaf node xs =
  case treeMapAtDepth' 0 leaf node xs of
    ([], s) -> Just s
    _ -> Nothing

main =
  do
    input@(hd : tl) <- map fst . fromJust . mapM parseFlat . lines <$> readFile "../input.txt"
    print $ magnitude $ foldl sumReduce hd tl
    print $ maximum $ map magnitude $ sumReduce <$> input <*> input
