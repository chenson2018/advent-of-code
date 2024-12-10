import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.List.Basic
open Std
open Std.HashSet
open Function
open Prod

namespace Day06

inductive Dir | N | E | S | W deriving Repr, Inhabited, DecidableEq
open Dir

def Dir.rotate (dir : Dir) :=
  match dir with
  | N => E
  | E => S
  | S => W
  | W => N

def Dir.op (dir : Dir) := dir.rotate.rotate

def Dir.next (x y : Nat) (dir : Dir) : Nat × Nat := 
  match dir with
  | N => (x-1,y)
  | E => (x,y+1)
  | S => (x+1,y)
  | W => (x,y-1)

structure State where
  x : Nat
  y : Nat
  visited : List (Dir × Nat × Nat)
  obstacles : List (Nat × Nat)
  dir : Dir
  range_x : Nat
  range_y : Nat
  halted : Bool
deriving Repr, Inhabited

-- assume correct shape, formatting, etc.
open List in
def State.from_list (grid : List (List Char)) : State := 
  let range_x := grid.length 
  let range_y := grid[0]!.length
  let idx := product (range range_x)  (range range_y)
  let with_idx:= grid.flatten.zip idx
  let guards := with_idx.filter ((·=='^') ∘ fst)
  let (_,x,y) := guards[0]!
  let obstacles := with_idx.filter ((·=='#') ∘ fst) |>.map snd
  {x, y, visited := [(N,x,y)], obstacles, dir := N, halted := false, range_x, range_y}

def State.tick (state : State) : State := 
  let {x,y,visited,obstacles,dir,range_x,range_y,..} := state
  let maybe_obs : Option (Nat × Nat) := 
    match dir with
    | N => obstacles.filter (λ (x',y') => x' < x  ∧ y = y') |>.argmax fst
    | S => obstacles.filter (λ (x',y') => x  < x' ∧ y = y') |>.argmin fst
    | W => obstacles.filter (λ (x',y') => y' < y  ∧ x = x') |>.argmax snd
    | E => obstacles.filter (λ (x',y') => y  < y' ∧ x = x') |>.argmin snd
  let (halted,x,y) :=
    match maybe_obs, dir with
    | none, N => (true,0,y)
    | none, S => (true,range_x - 1, y)
    | none, W => (true,x,0)
    | none, E => (true,x,range_y - 1)
    | some obs,_ => (false, uncurry dir.op.next obs)
  {state with x, y, halted, visited := (dir.rotate,x,y) :: visited, dir := dir.rotate}

partial def State.tick_all (state : State) :=
  if state.halted 
  then 
    some state
  else if (state.dir,state.x,state.y) ∈ state.visited.tail
  then
    none
  else 
    state.tick.tick_all

-- assumes either x or y is equal
def between (a b : Nat × Nat) := 
  let (x ,y ) := a
  let (x',y') := b
  let line high low := List.range (high-low+1) |>.map (· + low)
  if x = x' then 
    line (max y y') (min y y') |>.map (x,·)
  else 
    line (max x x') (min x x') |>.map (·,y)

def State.visited_unique (state : State) := 
  let idx := state.visited.map snd
  let pairs := idx.zip idx.tail
  let betweens := pairs.map (uncurry between)
  betweens |>.flatten |> Std.HashSet.ofList |>.size

def p2 (state : State) :=
 let new_obs := List.product (List.range state.range_x) (List.range state.range_y) |>.filter (· != (state.x,state.y))
 let end_states := new_obs.map (λ new => {state with obstacles := new :: state.obstacles}.tick_all)
 end_states |>.filter Option.isNone |>.length

@[aoc_main day_06]
def day_06 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename
  let grid := text.toList.map String.toList
  let state := State.from_list grid

  let p1_ans := state.tick_all.get!.visited_unique
  assert! p1_ans = 4559
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := p2 state
  assert! p2_ans = 1604
  println! s!"Part 2 answer: {p2_ans}"
