module Intcode where

import Control.Arrow
import Data.List (find)
import Data.Map
import Data.Maybe

-- some utilities
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

data Intcode = I
  { program :: Map Int Int,
    ptr :: Int
  }
  deriving (Show)

parseIntcode :: String -> Intcode
parseIntcode s = I {program = initMap s, ptr = 0}
  where
    initMap = fromList . zip [0 ..] . fmap read . wordsWhen (== ',')

iter :: Intcode -> Intcode
iter I {program, ptr} = I {program = insert d new_val program, ptr = ptr + 4}
  where
    a = program ! (ptr + 0)
    b = program ! (ptr + 1)
    c = program ! (ptr + 2)
    d = program ! (ptr + 3)
    val_b = program ! b
    val_c = program ! c
    new_val = case a of
      1 -> val_b + val_c
      2 -> val_b * val_c

runAll :: Intcode -> Intcode
runAll ins = fromJust $ find (\I {program, ptr} -> 99 == program ! ptr) $ iterate iter ins
