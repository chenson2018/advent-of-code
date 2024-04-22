-- sum [1 .. n]
nsum :: (Integral a) => a -> a
nsum i = (i ^ 2 + i) `div` 2

-- coordinate in the grid, starting with 1
coor :: (Integral a) => a -> a -> a
coor x y = y + nsum (x + y - 2)

-- take until a cyle is reached (first element is seen again)
untilCycle :: (Eq a) => (a -> a) -> a -> [a]
untilCycle = aux []
  where
    aux [] f seed = aux [f seed, seed] f seed
    aux xs@(hd : tl) f seed
      | hd == seed = reverse tl
      | otherwise = aux (f hd : xs) f seed

codegen :: (Integral a) => a -> a
codegen x = (252533 * x) `mod` 33554393

main :: IO ()
main =
  do
    let idx = coor 2978 3083
    let loop = untilCycle codegen 20151125
    print $ loop !! ((idx - 1) `mod` length loop)
