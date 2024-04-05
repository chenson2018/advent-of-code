import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Graph
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Parsing

data Line = Line
  { name :: String,
    weight :: Int,
    disc :: [String]
  }
  deriving (Show)

discP :: Parser [String]
discP =
  do
    symbol "->"
    list "," identifier
    <|> return []

line :: Parser Line
line =
  do
    name <- identifier
    symbol "("
    weight <- integer
    symbol ")"
    disc <- discP
    return Line {name, weight, disc}

getMaps :: [Line] -> (M.Map String Int, M.Map String [String])
getMaps xs = (weights, paths)
  where
    weights = M.fromList $ zip (map name xs) (map weight xs)
    paths = M.fromList $ filter ((/= []) . snd) $ zip (map name xs) (map disc xs)

roots :: (Eq a1) => M.Map a1 a2 -> M.Map k [a1] -> [a1]
roots weights paths = M.keys weights \\ concat (M.elems paths)

toTree :: (Ord a) => M.Map a [a] -> a -> Tree a
toTree paths root =
  case M.lookup root paths of
    Just xs -> Node root $ map (toTree paths) xs
    Nothing -> Node root []

-- the state is the correction offset and node to be corrected
imbalance :: Tree Int -> State (Maybe Int, Maybe Int) [Tree Int]
imbalance (Node v xs) =
  case grouping of
    [[unbal], bal : _] ->
      do
        (offset, _) <- get
        when (isNothing offset) $ put (Just $ unbal - bal, Nothing)
        imbalance $ xs !! fromJust (elemIndex unbal sums)
    _ ->
      do
        modify $ second (const $ Just v)
        return xs
  where
    sums = map sum xs
    grouping = sortOn length $ (group . sort) sums

main =
  do
    input <- map fst . fromJust . mapM (parse line) . lines <$> readFile "../input.txt"
    let (weights, paths) = getMaps input
    let [bot] = roots weights paths
    let weightTree = (weights M.!) <$> toTree paths bot
    let (Just offset, Just val) = execState (imbalance weightTree) (Nothing, Nothing)
    print bot
    print (val - offset)
