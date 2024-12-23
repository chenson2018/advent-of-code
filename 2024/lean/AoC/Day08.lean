import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.List.Basic
import Init.Data.Hashable
open Std.HashSet
open Prod Function

def List.pairs [BEq α] (xs : List α) : List (α × α) := product xs xs |>.filter (uncurry (·!=·))

namespace Day08

structure Grid where
  antennas : List ((Int × Int) × Char)
  frequencies : List Char
  range_x : Nat
  range_y : Nat
deriving Repr, Inhabited

open List Prod in
def Grid.ofList (grid : List (List Char)) : Grid := 
  let range_x := grid.length 
  let range_y := grid[0]!.length
  let idx := product (range range_x |>.map Int.ofNat)  (range range_y |>.map Int.ofNat)
  let with_idx := idx.zip grid.flatten
  let antennas := with_idx.filter ((·!='.') ∘ Prod.snd)
  let frequencies := antennas.map snd |> Std.HashSet.ofList |>.toList
  {antennas,frequencies,range_x,range_y}

def p1_antinodes (_ : Grid) (pair : (Int × Int) × (Int × Int)) := 
  let ((x,y),(x',y')) := pair
  let dx := x - x'
  let dy := y - y'
  [(x+dx,y+dy)]

-- TODO: a more specific version of this would terminate, but was annoying to prove
partial def iterWhile {α : Type} (f : α → α) (cond : α → Bool) (a : α) : List α := 
    if cond a then a :: iterWhile f cond (f a) else []

def p2_antinodes (grid : Grid) (pair : (Int × Int) × (Int × Int)) :=
  let ((x,y),(x',y')) := pair
  let dx := x - x'
  let dy := y - y'
  let cond x y : Bool := 0 ≤ x ∧ 0 ≤ y ∧ x < (grid.range_x : Int) ∧ y < (grid.range_y : Int)
  iterWhile (λ (x,y) => (x+dx,y+dy)) (uncurry cond) (x,y)

def Grid.antinodes (grid : Grid) (calc_anti : Grid → (Int × Int) × (Int × Int) → List (Int × Int)) :=
  grid.frequencies
    -- -- get all combinations of equal frequency
    |>.map (λ freq => grid.antennas.filter ((·==freq) ∘ snd) |>.map fst)
    |>.map List.pairs |>.flatten
    -- -- calculate the antinodes
    |>.map (calc_anti grid)
    |>.flatten 
    -- remove duplicates and points outside the grid
    |>.filter (λ (x,y) => 0 ≤ x ∧ 0 ≤ y ∧ x < grid.range_x ∧ y < grid.range_y)
    |> Std.HashSet.ofList

@[aoc_main day_08]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let grid := Grid.ofList $ text.toList.map String.toList
  
  let p1_ans := grid.antinodes p1_antinodes |>.size
  assert! p1_ans = 308
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := grid.antinodes p2_antinodes |>.size
  assert! p2_ans = 1147
  println! s!"Part 2 answer: {p2_ans}"
