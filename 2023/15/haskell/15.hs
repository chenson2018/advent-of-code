import Data.Char
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
parseInstruction s =
  case splitWhen (not . isAlpha) s of
    (label, '=' : num) ->
      I
        { label,
          op = Focal (read num),
          box = hash label
        }
    (label, "-") ->
      I
        { label,
          op = Minus,
          box = hash label
        }

-- hashmap for box states
type BoxMap = M.Map Int [(String, Int)]

initBox :: BoxMap
initBox = M.fromList $ map (,[]) [0 .. 255]

processInst :: Instruction -> BoxMap -> BoxMap
processInst (I {label, box, op = Minus}) = M.adjust (filter ((/= label) . fst)) box
processInst (I {label, box, op = Focal n}) = M.adjust (replaceOrAdd (\a a' -> fst a == fst a') (label, n)) box

p2 :: [Instruction] -> Int
p2 ins = score $ iter ins initBox
  where
    iter ins bm = foldl (flip processInst) bm ins
    score =
      M.foldrWithKey
        (\b xs acc -> acc + sum (zipWith (\s f -> (b + 1) * s * f) [1 ..] (map snd xs)))
        0

main = do
  raw <- readFile "../input.txt"
  let unparsed = split (takeWhile (/= '\n') raw) ','
  print (sum $ map hash unparsed)
  let parsed = map parseInstruction unparsed
  print (p2 parsed)
