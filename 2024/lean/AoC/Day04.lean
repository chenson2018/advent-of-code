import Batteries.Data.List.Basic
import AoC.CmdAttr
import AoC.Utils
open Std.HashMap
open Function

-- given a list, get its diagonals in one direction
-- op should be +/-, assumes a rectangle
def List.diags_general [Inhabited α] (xs : List (List α)) (op : Int → Int → Int) : List (List α) :=
  match xs with
  | [] => []
  | ys :: _ =>
    product (range xs.length) (range ys.length)
    |> foldl 
       (λ m (x,y) => 
          -- this should always be in bounds, but not sure how to prove here..
          let cons_elem diag := do let elem ← (←xs[x]?)[y]?; elem :: diag
          m.alter (op x y) (cons_elem ∘ flip Option.getD [])) empty 
    |> Std.HashMap.values

def List.diags [Inhabited α] (xs : List (List α)) := xs.diags_general (·+·) ++ xs.diags_general (·-·)
def List.contiguous_sub (xs : List α) (n : Nat) := range (xs.length - n + 1) |> map (take n ∘ (xs.drop ·))

def x_mas? (xs : List (List Char)) (x y : Nat) : Option Bool := do
  let center      ← (← xs[x    ]?)[    y]?
  let upper_left  ← (← xs[x - 1]?)[y - 1]?
  let upper_right ← (← xs[x - 1]?)[y + 1]?
  let lower_left  ← (← xs[x + 1]?)[y - 1]?
  let lower_right ← (← xs[x + 1]?)[y + 1]?
  let ys := [upper_left, upper_right, lower_left, lower_right]
  let two_M := ys.filter (· == 'M') |> List.length |> (· == 2)
  let two_S := ys.filter (· == 'S') |> List.length |> (· == 2)
  let corners := upper_left ≠ lower_right
  let center_A := center = 'A'
  pure $ two_M ∧ two_S ∧ corners ∧ center_A

open List in
def p2 (xs : List (List Char)) := 
  match xs with
  | [] => 0
  | ys :: _ =>
      let range_x := range (xs.length - 2) |> map (· + 1)
      let range_y := range (ys.length - 2) |> map (· + 1)
      product range_x range_y
      |> filter ((Option.getD · false) ∘ (uncurry $ x_mas? xs)) 
      |> List.length

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
    |> flatten |> filter (fun xs => xs = "XMAS".toList ∨ xs = "XMAS".toList.reverse)
    |> length
  assert! p1_ans = 2583
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := p2 mat
  assert! p2_ans = 1978
  println! s!"Part 2 answer: {p2_ans}"
