import Data.Foldable (maximumBy)
import Data.Function
import Data.Maybe

data Probe = Probe
  { x :: Int,
    y :: Int,
    vx :: Int,
    vy :: Int
  }
  deriving (Show)

initProbe :: Int -> Int -> Probe
initProbe vx vy = Probe {x = 0, y = 0, vx, vy}

data Target = Target
  { min_x :: Int,
    max_x :: Int,
    min_y :: Int,
    max_y :: Int
  }
  deriving (Show)

inTarget :: Target -> (Int, Int) -> Bool
inTarget t (x, y) = and [min_x t <= x, x <= max_x t, min_y t <= y, y <= max_y t]

step :: Probe -> Probe
step Probe {x, y, vx, vy}
  | vx < 0 = Probe {vx = vx + 1, x = new_x, y = new_y, vy = new_vy}
  | vx > 0 = Probe {vx = vx - 1, x = new_x, y = new_y, vy = new_vy}
  | vx == 0 = Probe {vx = vx, x = new_x, y = new_y, vy = new_vy}
  where
    new_x = x + vx
    new_y = y + vy
    new_vy = vy - 1

display :: Target -> [Probe] -> [[Char]]
display target@Target {min_x, min_y, max_x, max_y} probes = ret
  where
    probe_coors = map ((,) <$> x <*> y) probes
    minx = minimum $ 0 : min_x : map fst probe_coors
    maxx = maximum $ 0 : max_x : map fst probe_coors
    miny = minimum $ 0 : min_y : map snd probe_coors
    maxy = maximum $ 0 : min_y : map snd probe_coors

    coors = [[(x, y) | x <- [minx .. maxx]] | y <- [miny .. maxy]]

    disp coor
      | coor == (0, 0) = 'S'
      | coor `elem` probe_coors = '#'
      | inTarget target coor = 'T'
      | otherwise = '.'

    ret = reverse $ (map . map) disp coors

try :: Target -> Int -> Int -> Maybe ((Int, Int), Int)
try target vx vy = if inTarget target (px, py) then Just ((vx, vy), high) else Nothing
  where
    probes = takeWhile ((>= min_y target) . y) $ iterate step $ initProbe vx vy
    Probe {x = px, y = py} = last probes
    high = maximum $ map y probes

main =
  do
    -- let target = Target {min_x = 20, max_x = 30, min_y = -10, max_y = -5}
    let target = Target {min_x = 153, max_x = 199, min_y = -114, max_y = -75}
    let m = 150
    let combos = (,) <$> [(-m) .. m] <*> [(-m) .. m]
    let paths = mapMaybe (uncurry $ try target) combos
    print $ snd $ maximumBy (compare `on` snd) paths

-- this is wrong...
-- something is off with the indexing I think???
-- check the value above wwith fst instead of snd
-- print $ length paths
