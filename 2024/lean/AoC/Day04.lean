import Batteries.Data.List.Basic
import AoC.CmdAttr
import AoC.Utils
open Std.HashMap

-- given a list, get its diagonals in one direction
-- op should be +/-, assumes a rectangle
def List.diags_general [Inhabited α] (xs : List (List α)) (op : Int → Int → Int) : List (List α) := -- Id.run do
  let idx := (range xs.length).product $ range xs[0]!.length
  let m := idx.foldl (fun m (x,y) => m.alter (op x y) (λ ys => some (xs[x]![y]! :: ys.getD []))) empty
  m.values

def List.diags [Inhabited α] (xs : List (List α)) := xs.diags_general (·+·) ++ xs.diags_general (·-·)
def List.contiguous_sub (xs : List α) (n : Nat) := range (xs.length - n + 1) |> map (take n ∘ (xs.drop ·))

def x_mas? (xs : List (List Char)) (x y : Nat) : Bool := 
  let center_A := xs[x]![y]! == 'A'
  let upper_left  := xs[x - 1]![y - 1]!
  let upper_right := xs[x - 1]![y + 1]!
  let lower_left  := xs[x + 1]![y - 1]!
  let lower_right := xs[x + 1]![y + 1]!
  let ys := [upper_left, upper_right, lower_left, lower_right]
  let two_M := ys.filter (· == 'M') |> List.length |> (· == 2)
  let two_S := ys.filter (· == 'S') |> List.length |> (· == 2)
  let corners := upper_left ≠ lower_right
  two_M ∧ two_S ∧ corners ∧ center_A

open List in
def p2 (xs : List (List Char)) := 
  let range_x := range (xs.length - 2) |> map (· + 1)
  let range_y := range (xs[0]!.length - 2) |> map (· + 1)
  let idx := range_x.product range_y
  idx.filter (fun (x,y) => x_mas? xs x y) |> List.length

namespace Day04

open List in
@[aoc_main day_04]
def day_03 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename
  let mat := (text.map String.toList).toList

  let p1_ans := 
    mat ++ mat.transpose ++ mat.diags 
    |> map (contiguous_sub · 4) 
    |> join |> filter (fun xs => xs = ['X', 'M', 'A', 'S'] ∨ xs = ['S', 'A', 'M', 'X'])
    |> length
  assert! p1_ans = 2583
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := p2 mat
  assert! p2_ans = 1978
  println! s!"Part 2 answer: {p2_ans}"
