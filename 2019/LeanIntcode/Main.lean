import LeanIntcode

-- Day 2
def noun_verb (data : Array Nat) (noun verb : Nat) := do
  let data := data.set! 1 noun
  let data := data.set! 2 verb
  let vm := Intcode.new data
  let vm ← vm.run
  pure (vm.data.get! 0)

def day2 : IO Unit := do
  let input ← IO.FS.readFile "../02/input.txt"
  let data := Array.map String.toNat! (List.toArray (input.trim.splitOn (sep := ",")))
  -- part 1
  let p1 ← noun_verb data 12 2
  -- precomputed part 2
  let (p2_noun, p2_verb) := (76, 10)
  let p2 ← noun_verb data p2_noun p2_verb
  assert! p2 = 19690720
  println! s!"Day 2, Part 1 answer : {p1}"
  println! s!"Day 2, Part 2 answer : {p2_noun * 100 + p2_verb}"
  println! ""

def main : IO Unit := do
  day2
