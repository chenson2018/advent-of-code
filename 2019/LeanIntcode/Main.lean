import LeanIntcode

-- Day 2
def noun_verb (data : Array Int) (noun verb : Nat) := do
  let data := data.set! 1 noun
  let data := data.set! 2 verb
  let vm := Intcode.new data
  let vm ← vm.run
  pure (vm.data.get! 0)

def day2 : IO Unit := do
  let input ← IO.FS.readFile "../02/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  -- part 1
  let p1 ← noun_verb data 12 2
  -- precomputed part 2
  let (p2_noun, p2_verb) := (76, 10)
  let p2 ← noun_verb data p2_noun p2_verb
  assert! p1 = some 3765464
  assert! p2 = some 19690720
  println! s!"Day 2, Part 1 answer : {p1.get!}"
  println! s!"Day 2, Part 2 answer : {p2_noun * 100 + p2_verb}"
  println! ""

def day5 : IO Unit := do
  let input ← IO.FS.readFile "../05/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  let p1 ← (Intcode.new (input := [1]) (silent := true) data).run
  let p2 ← (Intcode.new (input := [5]) (silent := true) data).run
  assert! p1.get!.output.reverse = [0,0,0,0,0,0,0,0,0,7839346]
  assert! p2.get!.output.reverse = [447803]
  println! s!"Day 5, Part 1 answer : {p1.get!.output.head!}"
  println! s!"Day 5, Part 2 answer : {p2.get!.output.head!}"
  println! ""

-- Day 7
def day7 : IO Unit := do
  -- works for sample but not real input...
  let input ← IO.FS.readFile "../07/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))

  -- part 1
  let mut res := []
  for perm in [0, 1, 2, 3, 4].permutations do
    let mut out := []
    for phase in perm do
      let vm := Intcode.new data (input := [phase, out.headD 0]) (silent := true)
      let vm ← vm.run
      out := vm.get!.output.get! 0 :: out
    res := out.max? :: res
  
  let p1_ans := (List.reduceOption res).max?.get!
  assert! p1_ans = 65464
  println! s!"Day 7, Part 1 answer : {p1_ans}"

def main : IO Unit := do
  -- day2
  -- day5
  day7

