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

day_one :: String -> Int -> Int -> [[Int]]
day_one input group goal = do 
    --type conversion, then get/filter combinations
    --read file by lines
    let list  = map (read::String->Int) (lines input) 
        combo = subsequencesOfSize group list
        match = filter (\x -> (sum x) == goal) combo
        prod  = map    (\x -> product x)       match
    return prod

main = do 
    input <- readFile "../input.txt"
    print (day_one input 2 2020)
    print (day_one input 3 2020)
