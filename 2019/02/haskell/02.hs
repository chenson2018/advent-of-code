import Data.Map (insert, (!))
import Intcode

insertNounVerb :: Int -> Int -> Intcode -> Intcode
insertNounVerb noun verb I {program, ptr} = I {program = update program, ptr}
  where
    update = insert 2 verb . insert 1 noun

p1 :: Intcode -> Int
p1 ins = program (runAll $ insertNounVerb 12 2 ins) ! 0

p2 :: Intcode -> Int
p2 ins = n * 100 + v
  where
    (n, v) = head $ filter ((== 19690720) . tryProgram) possible
    tryProgram (n, v) = (! 0) $ program $ runAll $ insertNounVerb n v ins
    possible = (,) <$> [0 .. 100] <*> [0 .. 100]

main = do
  raw <- readFile "../input.txt"
  let intcode = parseIntcode raw
  print (p1 intcode)
  print (p2 intcode)
