import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.Array.Basic
import Batteries.Data.List.Basic
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

namespace Day15

inductive Dir | N | E | S | W deriving Repr, Inhabited, DecidableEq
open Dir

def Dir.next (x y : Nat) (dir : Dir) : Nat × Nat := 
  match dir with
  | N => (x-1,y)
  | E => (x,y+1)
  | S => (x+1,y)
  | W => (x,y-1)

structure State where
  coor : Nat × Nat
  walls : HashSet (Nat × Nat)
  boxes : HashSet (Nat × Nat)
  range_x : Nat
  range_y : Nat
deriving Repr

def State.GPS (s : State) :=
    s.boxes.toList.map (λ (x,y) ↦ 100 * x + y) |>.sum

-- annoying to prove termination for functions like these...
partial def State.moved_boxes_p1 (s : State) (coor : Nat × Nat) (dir : Dir) : Option (List (Nat × Nat)) := do
  let next := uncurry dir.next coor
  if s.boxes.contains next then
    next :: (← s.moved_boxes_p1 next dir)
  else if s.walls.contains next then 
    none
  else 
    some []

def State.tick_p1 (s : State) (dir : Dir) : State := 
  let coor := uncurry dir.next s.coor
  if s.walls.contains coor then
    s
  else if s.boxes.contains coor then
    match s.moved_boxes_p1 s.coor dir with
    | none => s
    | some bs =>
        let boxes := bs.foldl (HashSet.erase) s.boxes |>.insertMany (bs.map (uncurry dir.next))
        {s with coor, boxes}
  else
    {s with coor}

def Dir.ofChar (c : Char) : Except String Dir := 
  match c with
  | '^' => Except.ok N
  | 'v' => Except.ok S
  | '>' => Except.ok E
  | '<' => Except.ok W
  | _ => Except.error "invalid dir"

open List in
def parse (input : String) : Except String (State × List Dir) := do
  match input.splitOn "\n\n" with
  | [grid, movements] =>
    let grid := grid.splitOn "\n"
    let grid := grid.map String.toList
    let range_x := grid.length 
    let range_y := grid[0]!.length
    let idx := List.product (range range_x) (range range_y)
    let with_idx := grid.flatten.zip idx
    let boxes := with_idx.filter ((·=='O') ∘ fst) |>.map snd |> HashSet.ofList
    let walls := with_idx.filter ((·=='#') ∘ fst) |>.map snd |> HashSet.ofList
    let coor := with_idx.filter ((·=='@') ∘ fst) |>.map snd |>.get! 0
    let movements ← (movements.toList.filter (not ∘ Char.isWhitespace)).mapM Dir.ofChar
    pure ({coor,walls,boxes,range_x,range_y},movements)
  | _ => Except.error "invalid input"

def State.print (s : State) : IO Unit := do
  for x in [0:s.range_x] do
    for y in [0:s.range_y] do
      IO.print
        (if (x,y) = s.coor then
          '@'
        else if s.boxes.contains (x,y) then
          'O'
        else if s.walls.contains (x,y) then
          '#'
        else
          '.')
    println! ""

@[aoc_main day_15]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename 
  let (state,movements) ← parse text |> IO.ofExcept
  
  let p1_state := movements.foldl (State.tick_p1) state
  let p1_ans := p1_state.GPS
  assert! p1_ans = 1515788
  println! s!"Part 1 answer: {p1_ans}"
