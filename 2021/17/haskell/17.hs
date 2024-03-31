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
inTarget Target {min_x, max_x, min_y, max_y} (x, y) = and [min_x <= x, x <= max_x, min_y <= y, y <= max_y]

step :: Probe -> Probe
step Probe {x, y, vx, vy}
  | vx < 0 = Probe {vx = vx + 1, x = new_x, y = new_y, vy = new_vy}
  | vx > 0 = Probe {vx = vx - 1, x = new_x, y = new_y, vy = new_vy}
  | vx == 0 = Probe {vx = vx, x = new_x, y = new_y, vy = new_vy}
  where
    new_x = x + vx
    new_y = y + vy
    new_vy = vy - 1

try :: Target -> Int -> Int -> Maybe ((Int, Int), Int)
try target vx vy =
  case filt of
    [] -> Nothing
    _ -> Just ((vx, vy), high)
  where
    init = initProbe vx vy
    path = iterate step init
    -- this is a hack
    probes = take 350 path
    filt = filter (\Probe {x, y} -> inTarget target (x, y)) probes
    high = maximum $ map y probes

getIntersects :: Target -> [((Int, Int), Int)]
getIntersects target@Target {min_x, max_x, min_y, max_y} = paths
  where
    m = maximum $ map abs [min_x, max_x, min_y, max_y]
    paths = catMaybes $ try target <$> [(-m) .. m] <*> [(-m) .. m]

main =
  do
    let target = Target {min_x = 153, max_x = 199, min_y = -114, max_y = -75}
    let intersects = getIntersects target
    print $ snd $ maximumBy (compare `on` snd) intersects
    print $ length intersects
