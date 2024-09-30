import Data.Char
import Data.List
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Text.Read (readMaybe)

check = take 5 . map head . sortOn (negate . length) . group . sort . filter isAlpha

data Code = Code
  { text :: String,
    sid :: Int,
    checksum :: String
  }
  deriving (Show)

parse xs = do
  idx <- findIndex (== '[') xs
  let (hd, c) = splitAt idx xs
  let (sid, text) = partition isNumber hd
  sid :: Int <- readMaybe sid
  return Code {text, sid, checksum = filter isAlpha c}

valid Code {text, checksum} = check text == checksum

rotate :: Int -> Char -> Char
rotate shift c
  | c == '-' = ' '
  | otherwise = toEnum $ ((fromEnum c - 97 + shift) `mod` 26) + 97

decode :: Code -> String
decode Code {text, sid} = map (rotate sid) text

main = do
  Just input <- mapM parse . lines <$> readFile "../input.txt"
  let is_valid = filter valid input
  (print . sum . map sid) is_valid
  print $ fmap sid . find ((isInfixOf "pole") . decode) $ is_valid
