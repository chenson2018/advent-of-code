import Control.Monad.State.Lazy
import Data.Bifunctor
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Parsing

bag :: Parser String
bag =
  do
    c1 <- identifier
    c2 <- identifier
    symbol "bags" <|> symbol "bag"
    return (c1 ++ " " ++ c2)

line :: Parser (String, [(String, Int)])
line =
  do
    parent <- bag
    symbol "contain"
    let contain = do i <- integer; b <- bag; return (b, i)
    xs <- list "," contain
    return (parent, xs)

containsBag :: M.Map String [String] -> String -> [String] -> Bool
containsBag m goal [] = False
containsBag m goal (key : xs) = key == goal || containsBag m goal (m M.! key ++ xs)

p1 :: M.Map String [String] -> String -> Int
p1 m goal = subtract 1 $ length $ M.filterWithKey (\k _ -> containsBag m goal [k]) m

countBags :: M.Map String [(String, Int)] -> [(String, Int)] -> State Int [(String, Int)]
countBags m [] = state (([],) . subtract 1)
countBags m ((key, count) : xs) =
  do
    -- count the outer bags
    modify (+ count)
    -- get count copies of the bags that are inside
    let inner = map (second (* count)) $ m M.! key
    countBags m (inner ++ xs)

p2 :: M.Map String [(String, Int)] -> String -> Int
p2 m bag = execState (countBags m [(bag, 1)]) 0

main =
  do
    input <- M.fromList . map fst . fromJust . mapM (parse line) . lines <$> readFile "../input.txt"
    let bags_only = M.map (map fst) input
    print $ p1 bags_only "shiny gold"
    print $ p2 input "shiny gold"
