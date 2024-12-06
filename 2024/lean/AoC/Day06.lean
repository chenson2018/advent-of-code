import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.List.Basic
open Std
open Std.HashSet
open Function
open Prod

namespace Day06

inductive Dir | N | E | S | W deriving Repr, Inhabited
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
  visited : List (Nat × Nat)
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
  let with_idx:= grid.join.zip idx
  let guards := with_idx.filter ((·=='^') ∘ fst)
  let (_,x,y) := guards[0]!
  let obstacles := with_idx.filter ((·=='#') ∘ fst) |>.map snd
  {x, y, visited := [(x,y)], obstacles, dir := N, halted := false, range_x, range_y}

def State.tick (state : State) : State := 
  let {x,y,visited,obstacles,dir,range_x,range_y,..} := state
  let maybe_obs : Option (Nat × Nat) := 
    match dir with
    | N => obstacles.filter (λ (x',y') => x' < x  ∧ y = y') |>.argmax fst
    | S => obstacles.filter (λ (x',y') => x  < x' ∧ y = y') |>.argmin fst
    | W => obstacles.filter (λ (x',y') => y' < y  ∧ x = x') |>.argmax snd
    | E => obstacles.filter (λ (x',y') => y  < y' ∧ x = x') |>.argmin snd
  let ((x,y),halted) :=
    match maybe_obs, dir with
    | none, N => ((0,y),true)
    | none, S => ((range_x - 1, y),true)
    | none, W => ((x,0),true)
    | none, E => ((x,range_y - 1),true)
    | some obs,_ => (uncurry dir.op.next obs,false)
  {state with x, y, halted, visited := (x,y) :: visited, dir := dir.rotate}

def State.tick_all_gas (state : State) (gas : Nat) := 
  match gas, state.halted with
  | _,true => some state
  | gas'+1,_ => state.tick.tick_all_gas gas'
  | 0,_ => none

-- assumes either x or y is equal
def between (a b : Nat × Nat) := 
  let (x ,y ) := a
  let (x',y') := b
  if x = x' 
  then 
    let high := max y y'
    let low  := min y y'
    List.range (high-low+1) |>.map (· + low) |>.map (x,·)
  else 
    let high := max x x'
    let low  := min x x'
    List.range (high-low+1) |>.map (· + low) |>.map (·,y)

def State.visited_unique (state : State) := 
  let pairs := state.visited.zip state.visited.tail
  let betweens := pairs.map (uncurry between)
  betweens |>.join |> Std.HashSet.ofList |>.size

def p2 (state : State) (gas_guess : Nat) :=
 let new_obs := List.product (List.range state.range_x) (List.range state.range_y) |>.filter (· != (state.x,state.y))
 let end_states := new_obs.map (λ new => {state with obstacles := new :: state.obstacles}.tick_all_gas gas_guess)
 end_states |>.filter Option.isNone |>.length

@[aoc_main day_06]
def day_06 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename
  let grid := text.toList.map String.toList
  let state := State.from_list grid

  -- a bit of a hack, so I don't have to properly check repeating states
  let gas_guess := 200

  let p1_ans := (state.tick_all_gas gas_guess).get!.visited_unique
  assert! p1_ans = 4559
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := p2 state 200
  assert! p2_ans = 1604
  println! s!"Part 2 answer: {p2_ans}"
