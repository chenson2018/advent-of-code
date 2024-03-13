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

data Score
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord)

data Player = P {cards :: [Card], bid :: Int}
  deriving (Show)

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

-- general scoring
cardsScoreLe :: ([Card] -> Score) -> [Card] -> [Card] -> Bool
cardsScoreLe metric c1 c2
  | s1 == s2 = tiebreakLe c1 c2
  | otherwise = s1 <= s2
  where
    s1 = metric c1
    s2 = metric c2

-- type and ordering for a standard hand
newtype Hand = H [Card] deriving (Show, Eq)

instance Ord Hand where
  (H c1) <= (H c2) = cardsScoreLe scoreHand c1 c2

-- type and ordering for Joker Scoring

-- this is a little overengineered, just experimenting
replaceCard :: Card -> a -> (Card -> a) -> Card -> a
replaceCard match_card cons f_alt given_card
  | match_card == given_card = cons
  | otherwise = f_alt given_card

replaceJoker :: Player -> Player
replaceJoker P {cards, bid} = P {cards = map (replaceCard J Joker id) cards, bid}

scoreJoker :: [Card] -> Score
scoreJoker cards =
  -- if all Jokers, this is empty, so a five of a kind
  case possibleScores of
    [] -> FiveKind
    _ -> maximum possibleScores
  where
    -- we only need to add possibilites that match some current card
    options = filter (`elem` cards) $ map N [2 .. 9] ++ [T, J, Q, K, A]
    [a, b, c, d, e] = map (replaceCard Joker options (: [])) cards
    possible = (\a b c d e -> [a, b, c, d, e]) <$> a <*> b <*> c <*> d <*> e
    possibleScores = map scoreHand possible

newtype JokerHand = JH [Card] deriving (Show, Eq)

instance Ord JokerHand where
  (JH c1) <= (JH c2) = cardsScoreLe scoreJoker c1 c2

-- answers for both parts

-- given scoring scheme for cards, calculate winnings
winnings :: (Ord a) => ([Card] -> a) -> [Player] -> Int
winnings f xs = sum $ zipWith (*) [1 ..] (map bid $ sortOn (f . cards) xs)

p1 :: [Player] -> Int
p1 = winnings H

p2 :: [Player] -> Int
p2 xs = winnings JH (map replaceJoker xs)

-- Parsing, should really use Read?

cardParse :: Char -> Card
cardParse 'A' = A
cardParse 'K' = K
cardParse 'Q' = Q
cardParse 'J' = J
cardParse 'T' = T
cardParse c = N (read [c])

parse :: String -> [Player]
parse raw = map parse_player raw_split
  where
    raw_split = map words $ lines raw
    parse_player xs =
      case xs of
        [raw_cards, raw_bid] -> P {cards = map cardParse raw_cards, bid = read raw_bid}

main :: IO ()
main = do
  raw <- readFile "../input.txt"
  let input = parse raw
  print (p1 input)
  print (p2 input)
