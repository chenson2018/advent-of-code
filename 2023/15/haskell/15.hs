import Control.Arrow
import Data.Char
import Data.Function
import qualified Data.Map as M

-- some utilities, some a bit inefficient with reversing strings

-- split a string by a given character
split' :: Char -> String -> String -> [String]
split' c [] partial = [reverse partial]
split' c (hd : tl) partial
  | c == hd = reverse partial : split' c tl []
  | otherwise = split' c tl (hd : partial)

split :: String -> Char -> [String]
split s c = split' c s ""

-- split a list on a predicate
splitWhen' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
splitWhen' f (x : xs) partial
  | f x = (reverse partial, x : xs)
  | otherwise = splitWhen' f xs (x : partial)

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen f xs = splitWhen' f xs []

-- general replacement in a list
replaceOrAdd :: (a -> a -> Bool) -> a -> [a] -> [a]
replaceOrAdd f a [] = [a]
replaceOrAdd f a (a' : tl)
  | f a a' = a : tl
  | otherwise = a' : replaceOrAdd f a tl

-- hash function for part 1
hash' :: String -> Int -> Int
hash' [] val = val
hash' (c : xs) val = hash' xs next
  where
    next = (17 * (val + ord c)) `mod` 256

hash :: String -> Int
hash s = hash' s 0

-- further interpretation for part 2
data Op = Minus | Focal Int deriving (Show)

data Instruction = I
  { label :: String,
    op :: Op,
    box :: Int
  }
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = I {label, box, op}
  where
    box = hash label
    (label, op) = case splitWhen (not . isAlpha) s of
      (label, '=' : num) -> (label, Focal (read num))
      (label, "-") -> (label, Minus)

-- hashmap for box states
type BoxMap = M.Map Int [Instruction]

initBox :: BoxMap
initBox = M.fromList $ map (,[]) [0 .. 255]

processInst :: BoxMap -> Instruction -> BoxMap
processInst bm (I {label = l, box, op = Minus}) = M.adjust (filter ((/= l) . label)) box bm
processInst bm ins@(I {box, op = Focal _}) = M.adjust (replaceOrAdd ((==) `on` label) ins) box bm

score :: BoxMap -> Int
score =
  M.elems
    >>> concatMap (zip [1 ..])
    >>> foldr (\(slot, I {box, op = Focal focal}) acc -> acc + (box + 1) * slot * focal) 0

p2 :: [Instruction] -> Int
p2 = score . foldl processInst initBox

main = do
  raw <- readFile "../input.txt"
  let unparsed = split (takeWhile (/= '\n') raw) ','
  print (sum $ map hash unparsed)
  let parsed = map parseInstruction unparsed
  print (p2 parsed)
