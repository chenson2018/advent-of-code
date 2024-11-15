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
def amplify (data : Array Int) (phases output : List Int) := do
  match phases with
  | phase :: phases =>
    let vm := Intcode.new data (input := [phase, List.headD output 0]) (silent := true)
    let vm ← vm.run
    let out := vm.output.head!
    pure (max out (← amplify data phases (out :: output)))
  | _ =>
    pure 0

def List.perm (xs : List α) : List (List α) :=
  match xs with
  | [] => [[]]
  | hd :: tl => List.map (List.cons hd) (tl.perm)

def day7 : IO Unit := do
  let input ← IO.FS.readFile "../07/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  let data := #[3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
  let amps ← List.mapM (fun perm => (amplify data perm []).run) [0, 1, 2, 3, 4].permutations
  let res := (List.reduceOption amps).max?
  println! res

def main : IO Unit := do
  day2
  day5
  day7

