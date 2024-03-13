import Data.List

data Card
  = Joker
  | N Int
  | T
  | J
  | Q
  | K
  | A
  deriving (Show, Eq, Ord)

-- this should really be Read
cardParse :: Char -> Card
cardParse 'A' = A
cardParse 'K' = K
cardParse 'Q' = Q
cardParse 'J' = J
cardParse 'T' = T
cardParse c = N (read [c])

data Score
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord)

freq :: (Ord a) => [a] -> [Int]
freq xs = sort $ map length $ (group . sort) xs

-- assumes proper length of 5 cards
scoreHand :: [Card] -> Score
scoreHand cards =
  case freq cards of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2] -> OnePair
    [1, 2, 2] -> TwoPair
    [1, 1, 3] -> ThreeKind
    [2, 3] -> FullHouse
    [1, 4] -> FourKind
    [5] -> FiveKind

tiebreakLe :: [Card] -> [Card] -> Bool
tiebreakLe [] [] = True
tiebreakLe (x : xs) (y : ys)
  | x == y = tiebreakLe xs ys
  | otherwise = x <= y

newtype Hand = H [Card] deriving (Show, Eq)

instance Ord Hand where
  (H c1) <= (H c2)
    | s1 == s2 = tiebreakLe c1 c2
    | otherwise = s1 <= s2
    where
      s1 = scoreHand c1
      s2 = scoreHand c2

data Player = P {hand :: Hand, bid :: Int}
  deriving (Show)

parse :: String -> [Player]
parse raw = map parse_player raw_split
  where
    raw_split = map words $ lines raw
    parse_player xs =
      case xs of
        [raw_cards, raw_bid] -> P {hand = H (map cardParse raw_cards), bid = read raw_bid}

p1 :: [Player] -> Int
p1 xs = sum $ zipWith (*) [1 ..] (map bid $ sortOn hand xs)

main :: IO ()
main = do
  -- raw <- readFile "../test.txt"
  raw <- readFile "../input.txt"
  let input = parse raw
  print (p1 input)
