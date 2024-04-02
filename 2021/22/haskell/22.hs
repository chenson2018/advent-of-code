import Data.Bifunctor
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Parsing

data Turn
  = Off
  | On
  deriving (Show)

data Ins = Ins
  { turn :: Turn,
    x_range :: (Int, Int),
    y_range :: (Int, Int),
    z_range :: (Int, Int)
  }
  deriving (Show)

turnP :: Parser Turn
turnP = Off <$ symbol "off" <|> On <$ symbol "on"

range :: Parser (Int, Int)
range = do
  l <- integer
  symbol ".."
  r <- integer
  return (l, r)

ins :: Parser Ins
ins = do
  turn <- turnP
  symbol "x="
  x_range <- range
  symbol ",y="
  y_range <- range
  symbol ",z="
  z_range <- range
  return Ins {turn, x_range, y_range, z_range}

processIns :: Set.Set (Int, Int, Int) -> Ins -> Set.Set (Int, Int, Int)
processIns s Ins {turn, x_range = (x_min, x_max), y_range = (y_min, y_max), z_range = (z_min, z_max)} = foldl (flip $ op turn) s combos
  where
    combos = (,,) <$> [x_min .. x_max] <*> [y_min .. y_max] <*> [z_min .. z_max]
    op On = Set.insert
    op Off = Set.delete

process :: [Ins] -> Set.Set (Int, Int, Int)
process = foldl processIns Set.empty

truncRange :: Int -> (Int, Int) -> Maybe (Int, Int)
truncRange limit (low, high)
  | high < (-limit) = Nothing
  | low > limit = Nothing
  | otherwise = Just (max (-limit) low, min limit high)

trunc :: Int -> Ins -> Maybe Ins
trunc limit Ins {turn, x_range, y_range, z_range} =
  do
    x <- truncRange limit x_range
    y <- truncRange limit y_range
    z <- truncRange limit z_range
    return $ Ins {turn, x_range = x, y_range = y, z_range = z}

main =
  do
    input <- map fst . fromJust . mapM (parse ins) . lines <$> readFile "../test.txt"
    input <- map fst . fromJust . mapM (parse ins) . lines <$> readFile "../input.txt"
    print $ length $ foldl processIns Set.empty $ mapMaybe (trunc 50) input
