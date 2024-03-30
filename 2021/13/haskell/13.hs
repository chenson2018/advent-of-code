import Data.Coerce
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Parsing

coor :: Parser (Int, Int)
coor =
  do
    x <- integer
    symbol ","
    y <- integer
    return (x, y)

data Fold
  = X Int
  | Y Int
  deriving (Show)

doFold :: Fold -> Set.Set (Int, Int) -> Set.Set (Int, Int)
doFold (Y yf) s = ret
  where
    above = Set.filter ((< yf) . snd) s
    below = Set.filter ((yf <) . snd) s
    flip_below = Set.map (\(x, y) -> (x, y - 2 * (y - yf))) below
    ret = Set.union above flip_below
doFold (X xf) s = ret
  where
    left = Set.filter ((< xf) . fst) s
    right = Set.filter ((xf <) . fst) s
    flip_right = Set.map (\(x, y) -> (x - 2 * (x - xf), y)) right
    ret = Set.union left flip_right

display :: (Show a) => a -> a -> Set.Set (Int, Int) -> [[a]]
display one zero s = disp
  where
    max_x = maximum $ Set.map fst s
    max_y = maximum $ Set.map snd s
    coors = [[(x, y) | x <- [0 .. max_x]] | y <- [0 .. max_y]]
    disp = (map . map) (\key -> if Set.member key s then one else zero) coors

{- ORMOLU_DISABLE -}

foldP :: Parser Fold
foldP =
  do symbol "fold along x="
     X <$> integer
  <|>
  do symbol "fold along y="
     Y <$> integer

{- ORMOLU_ENABLE -}

parseInput :: String -> (Set.Set (Int, Int), [Fold])
parseInput s = (c, f)
  where
    (raw_coor, _ : raw_fold) = (\xs -> splitAt (fromJust $ elemIndex "" xs) xs) . lines $ s
    c = Set.fromList $ map fst $ fromJust $ mapM (parse coor) raw_coor
    f = map fst $ fromJust $ mapM (parse foldP) raw_fold

main =
  do
    (coors, folds) <- parseInput <$> readFile "../input.txt"
    print $ length $ doFold (head folds) coors
    mapM print $ display '#' '.' $ foldr doFold coors $ reverse folds
