#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import System.IO
import Data.List

--see https://stackoverflow.com/a/58511843
--combinations of size n
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if (n > l) then []
                             else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                               in zipWith (++)
                                    ([]:next)
                                    ( map (map (x:)) next ++ [[]] )  

day_one :: String -> Int -> Integer -> IO ()
day_one file group goal = do
    --read file by lines
    handle     <- openFile file ReadMode
    contents   <- hGetContents handle
    --type conversion, then get/filter combinations
    let list  = map (read::String->Integer) (lines contents)
        combo = subsequencesOfSize group list
        match = filter (\x -> (sum x) == goal) combo
        prod  = map (\x -> product x) match
    print prod

main = do 
    day_one "../input.txt" 2 2020
    day_one "../input.txt" 3 2020
