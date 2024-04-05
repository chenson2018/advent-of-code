import Data.Function

generator :: Int -> Int -> Int
generator factor value = (factor * value) `mod` 2147483647

judgeCount :: Int -> Int -> Int -> Int -> (Int -> Bool) -> (Int -> Bool) -> Int -> Int
judgeCount factor_a factor_b init_a init_b filt_a filt_b limit = ret
  where
    ga = generator factor_a
    gb = generator factor_b
    as = filter filt_a $ iterate ga init_a
    bs = filter filt_b $ iterate gb init_b
    comp = zipWith ((==) `on` (`mod` 2 ^ 16)) as bs
    ret = length $ filter id $ take limit comp

main =
  do
    print $ judgeCount 16807 48271 591 393 (const True) (const True) 40_000_000
    print $ judgeCount 16807 48271 591 393 ((== 0) . (`mod` 4)) ((== 0) . (`mod` 8)) 5_000_000
