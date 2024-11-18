import LeanIntcode.Intcode
import LeanIntcode.Aux

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
  println! s!"Day 2, Part 1 answer: {p1.get!}"
  println! s!"Day 2, Part 2 answer: {p2_noun * 100 + p2_verb}"
  println! ""

def day5 : IO Unit := do
  let input ← IO.FS.readFile "../05/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  let p1 ← (Intcode.new (input := [1]) (silent := true) data).run
  let p2 ← (Intcode.new (input := [5]) (silent := true) data).run
  assert! p1.get!.output.reverse = [0,0,0,0,0,0,0,0,0,7839346]
  assert! p2.get!.output.reverse = [447803]
  println! s!"Day 5, Part 1 answer: {p1.get!.output.head!}"
  println! s!"Day 5, Part 2 answer: {p2.get!.output.head!}"
  println! ""

-- Day 7
-- experimenting with the imperative features
def day7 : IO Unit := do
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
  println! s!"Day 7, Part 1 answer: {p1_ans}"

  -- part 2
  let mut res2 := []

  for perm in [5, 6, 7, 8, 9].permutations do
    let mut amps : Array Intcode := #[]
    let mut signal := 0

    for phase in perm do
      let vm := Intcode.new data (input := [phase, signal]) (silent := true)
      let (vm, signal') := (← vm.run_until_output).get!
      signal := signal'
      amps := amps.push vm

    -- set the first signal
    amps := amps.set! 0 (let v := amps.get! 0; {v with input := [signal]})

    -- pure signals
    let mut idx := 0
    while !(amps.get! 4).halted do
      let (vm,signal') := (← (amps.get! idx).run_until_output).get!
      signal := signal'
      amps := amps.set! idx vm
      let next_idx := (idx + 1) % 5
      let next_vm  := amps.get! next_idx
      amps := amps.set! next_idx {next_vm with input := signal :: next_vm.input}
      idx := (idx + 1) % 5
    res2 := signal :: res2
  
  let p2_ans := res2.max?.get!
  assert! p2_ans = 1518124
  println! s!"Day 7, Part 2 answer: {p2_ans}"
  println! ""

def day9 : IO Unit := do
  let input ← IO.FS.readFile "../09/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  let vm ← (Intcode.new data (input := [1]) (silent := true)).run
  let p1_ans := vm.get!.output.get! 0
  let vm ← (Intcode.new data (input := [2]) (silent := true)).run
  let p2_ans := vm.get!.output.get! 0
  assert! p1_ans = 3765554916
  assert! p2_ans = 76642
  println! s!"Day 9, Part 1 answer: {p1_ans}"
  println! s!"Day 9, Part 2 answer: {p2_ans}"
  println! ""

-- Day 11
inductive Direction | Up | Down | Left | Right deriving Repr, Inhabited
inductive Color | Black | White deriving Repr, DecidableEq
inductive Turn | Clockwise | Counterclockwise

structure PaintingRobot where
  direction : Direction
  vm : Intcode
  x : Int
  y : Int
  painted : Std.HashMap (Int × Int) Color
deriving Repr, Inhabited

open Color Direction Turn in
def PaintingRobot.tick (robot : PaintingRobot) : OptionT IO PaintingRobot := do
  let current_color := Std.HashMap.getD robot.painted (robot.x, robot.y) Black
  let vm := {robot.vm with input := [if current_color = Black then 0 else 1]}
  
  let (vm,color_int) ← vm.run_until_output
  let (vm,turn_int) ← vm.run_until_output
  
  let color := if color_int = 0 then Black else White
  let painted := robot.painted.insert (robot.x, robot.y) color
  
  let turn  := if turn_int  = 0 then Counterclockwise else Clockwise
  
  let direction := 
    match robot.direction, turn with
    | Up   , Clockwise        => Right
    | Right, Clockwise        => Down
    | Down , Clockwise        => Left
    | Left , Clockwise        => Up
    | Up   , Counterclockwise => Left
    | Left , Counterclockwise => Down
    | Down , Counterclockwise => Right
    | Right, Counterclockwise => Up
  
  let (x,y) := 
    match direction with
    | Up    => (robot.x, robot.y + 1)
    | Down  => (robot.x, robot.y - 1)
    | Right => (robot.x + 1, robot.y)
    | Left  => (robot.x - 1, robot.y)
  
  pure {x, y, direction, vm, painted}

-- from Mathlib
def Int.range (m n : Int) : List Int :=
  ((List.range (toNat (n - m))) : List Nat).map fun (r : Nat) => (m + r : Int)

open Color Direction Turn in
def day11 : IO Unit := do
  let input ← IO.FS.readFile "../11/input.txt"
  let data := Array.map String.toInt! (List.toArray (input.trim.splitOn (sep := ",")))
  
  -- part 1
  let robot : PaintingRobot := {
    vm := Intcode.new data (silent := true), 
    direction := Up, 
    x := 0, y := 0, 
    painted := Std.HashMap.empty}
  let robot ← (bindUntil (Intcode.halted ∘ PaintingRobot.vm) PaintingRobot.tick robot).run
  let p1_ans := robot.get!.painted.size
  assert! p1_ans = 1747
  println! s!"Day 11, Part 1 answer: {p1_ans}"
  
  -- part 2
  let robot : PaintingRobot := {
    vm := Intcode.new data (silent := true), 
    direction := Up, 
    x := 0, y := 0, 
    painted := Std.HashMap.ofList [((0, 0), White)]}
  let robot ← (bindUntil (Intcode.halted ∘ PaintingRobot.vm) PaintingRobot.tick robot).run

  let painted := robot.get!.painted
  let x_min := (painted.keys.map Prod.fst).min?.get!
  let x_max := (painted.keys.map Prod.fst).max?.get!
  let y_min := (painted.keys.map Prod.snd).min?.get!
  let y_max := (painted.keys.map Prod.snd).max?.get!

  println! s!"Day 11, Part 2 answer:"
  for y in (Int.range y_min (y_max+1)).reverse do
    for x in Int.range x_min (x_max+1) do
      let color := painted.getD (x, y) Black
      IO.print (if color = Black then " " else "█")
    println! "" 
  println! ""

def main : IO Unit := do
  day2
  day5
  day7
  day9
  day11
