import qualified Data.Map as M
import Data.Maybe
import Parsing

data RegisterName
  = A
  | B
  deriving (Show)

data Instruction
  = Hlf RegisterName
  | Tpl RegisterName
  | Inc RegisterName
  | Jmp Int
  | Jie RegisterName Int
  | Jio RegisterName Int
  | Halt
  deriving (Show)

data Registers = R {a :: Int, b :: Int}
  deriving (Show)

updateRegister :: (Int -> Int) -> RegisterName -> Registers -> Registers
updateRegister f A R {a, b} = R {a = f a, b}
updateRegister f B R {a, b} = R {a, b = f b}

lookupRegister :: Registers -> RegisterName -> Int
lookupRegister R {a, b} A = a
lookupRegister R {a, b} B = b

type Program = M.Map Int Instruction

-- numbers and adds a Halt instruction
initProgram :: [Instruction] -> Program
initProgram ins = M.fromList $ zip [0 ..] $ ins ++ [Halt]

-- parsers
regname :: Parser RegisterName
regname = (A <$ char 'a') <|> (B <$ char 'b')

{- ORMOLU_DISABLE -}

sign :: Parser Int
sign = do s <- -1 <$ symbol "-" <|> 
                1 <$ symbol "+"
          val <- integer
          return (s * val)

ins :: Parser Instruction
ins =
  do symbol "hlf"
     Hlf <$> regname
  <|> 
  do symbol "tpl"
     Tpl <$> regname
  <|> do
      symbol "inc"
      Inc <$> regname
  <|> do
      symbol "jmp"
      Jmp <$> sign
  <|> do
      symbol "jie"
      r <- regname
      symbol ","
      Jie r <$> sign
  <|> do
      symbol "jio"
      r <- regname
      symbol ","
      Jio r <$> sign

{- ORMOLU_ENABLE -}

run :: Program -> Int -> Registers -> Maybe Registers
run program line registers =
  case M.lookup line program of
    Just Halt -> Just registers
    Just (Hlf r) -> run program (line + 1) $ updateRegister (`div` 2) r registers
    Just (Tpl r) -> run program (line + 1) $ updateRegister (* 3) r registers
    Just (Inc r) -> run program (line + 1) $ updateRegister (+ 1) r registers
    Just (Jmp off) -> run program (line + off) registers
    Just (Jie r off) ->
      if even $ lookupRegister registers r
        then run program (line + off) registers
        else run program (line + 1) registers
    Just (Jio r off) ->
      if (== 1) $ lookupRegister registers r
        then run program (line + off) registers
        else run program (line + 1) registers
    Nothing -> Nothing

main = do
  program <- initProgram . parseListJust ins . lines <$> readFile "../input.txt"
  print (run program 0 R {a = 0, b = 0})
  print (run program 0 R {a = 1, b = 0})
