import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Maybe
import Parsing

withDefault :: a -> (a -> a) -> (Maybe a -> Maybe a)
withDefault d f Nothing = Just $ f d
withDefault _ f (Just a) = Just $ f a

data Ins = Ins
  { target :: String,
    change :: Int -> Int,
    condReg :: String,
    cond :: Int -> Bool
  }

{- ORMOLU_DISABLE -}

ins :: Parser Ins
ins = 
  do
    target <- many lower
    f <- 
      (+) <$ symbol "inc" <|> 
      subtract <$ symbol "dec"
    amount <- integer
    let change = f amount
    symbol "if"
    condReg <- many lower
    f <- 
      (==) <$ symbol "==" <|>
      (<=) <$ symbol "<=" <|>
      (/=) <$ symbol "!=" <|>
      (>=) <$ symbol ">=" <|>
      (>) <$ symbol ">" <|>
      (<) <$ symbol "<"
    amount <- integer
    let cond = flip f amount
    return Ins {target, change, condReg, cond}

{- ORMOLU_DISABLE -}

process :: M.Map String Int -> Ins -> M.Map String Int
process m Ins {target, change, condReg, cond} =
  if cond $ M.findWithDefault 0 condReg m
  then
    M.alter (withDefault 0 change) target m
  else 
    m

-- generic over
--  `extract` - that does some calculation over the registers
--  `comp`    - decides how to combine the results of successive `extract`
registerFold :: (M.Map String Int -> a) -> (a -> a -> a) -> [Ins] -> State (M.Map String Int, a) [Ins]
registerFold extract comp (ins : tl) = 
  do (m, global) <- get
     let m' = process m ins
     let global' = comp global (extract m')
     put (m', global')
     registerFold extract comp tl
registerFold _ _ [] = state ([],)

main = 
  do 
     input <- map fst . fromJust . mapM (parse ins) . lines <$> readFile "../input.txt"
     let (m, global) = execState (registerFold maximum max input) (M.empty, 0)
     print $ maximum m
     print global
