import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Maybe (fromJust)
import Parsing

-- parse the trees into a flat list of depths and values
type Flat t = (Int, t)

{- ORMOLU_DISABLE -}

flat :: Int -> Parser t -> Parser [Flat t]
flat depth parseT =
  do
    val <- parseT
    return [(depth, val)]
  <|> 
  do
    symbol "["
    l <- flat (depth + 1) parseT
    symbol ","
    r <- flat (depth + 1) parseT
    symbol "]"
    return $ l ++ r

{- ORMOLU_ENABLE -}

parseFlatInt :: String -> Maybe ([Flat Int], String)
parseFlatInt = parse $ flat 0 integer

-- splitting calculation
divRound :: Int -> (Int, Int)
divRound i
  | even i = (d, d)
  | odd i = (d, d + 1)
  where
    d = i `div` 2

-- single step of a reduction
-- note the tricky order of pattern matching

{- ORMOLU_DISABLE -}

step :: [Flat Int] -> [Flat Int]
step (          (5, el) : (5, er) : (dr, r) : tl) =                (4, 0) : (dr, er + r) : tl
step ((dl, l) : (5, el) : (5, er) : (dr, r) : tl) = (dl, el + l) : (4, 0) : (dr, er + r) : tl
step ((dl, l) : (5, el) : (5, er)           : tl) = (dl, el + l) : (4, 0) :                tl
step (x : xs)
  | snd x >= 10 = bimap (+ 1) (fst . divRound) x : bimap (+ 1) (snd . divRound) x : xs
  | otherwise = x : step xs
step [] = []

{- ORMOLU_ENABLE -}

-- generic iterate that stops on consecutive repeated value
iterateUntilRepeat :: (Eq a) => (a -> a) -> a -> [a]
iterateUntilRepeat f init
  | init == next = [init]
  | otherwise = init : iterateUntilRepeat f next
  where
    next = f init

-- reduction steps until stable
reduction :: [Flat Int] -> [Flat Int]
reduction = last . iterateUntilRepeat step

-- add and reduce two values
sumReduce :: [Flat Int] -> [Flat Int] -> [Flat Int]
sumReduce a b = reduction $ map (first (+ 1)) $ a ++ b

-- this generally accumulates a generic State from [Flat] according to its tree structure
-- parameters leaf and node are functions to handle each case
-- inspired by `unflatten` at: https://www.reddit.com/r/haskell/comments/rizwa7/comment/hp14g7i/

treeMapAtDepth :: Int -> (t -> s) -> (s -> s -> s) -> [Flat t] -> State (Maybe s) [Flat t]
treeMapAtDepth depth leaf node xs@((d, v) : tl) =
  if depth == d
    then do
      put $ Just $ leaf v
      return tl
    else do
      let next = treeMapAtDepth (depth + 1) leaf node
      xs <- next xs
      l <- get
      xs <- next xs
      r <- get
      put $ node <$> l <*> r
      return xs
treeMapAtDepth _ _ _ [] = state ([],)

treeMap :: (t -> s) -> (s -> s -> s) -> [Flat t] -> Maybe s
treeMap leaf node xs =
  case runState (treeMapAtDepth 0 leaf node xs) Nothing of
    ([], Just s) -> Just s
    _ -> Nothing

magnitude :: [Flat Int] -> Maybe Int
magnitude = treeMap id (\l r -> 3 * l + 2 * r)

-- Tree representation
-- not used, just an example of the generality of treeMap
data Tree t
  = Val t
  | Pair (Tree t) (Tree t)
  deriving (Show)

unflatten :: [Flat t] -> Maybe (Tree t)
unflatten = treeMap Val Pair

main =
  do
    input@(hd : tl) <- map fst . fromJust . mapM parseFlatInt . lines <$> readFile "../input.txt"
    print $ magnitude $ foldl sumReduce hd tl
    print $ maximum $ map magnitude $ sumReduce <$> input <*> input
