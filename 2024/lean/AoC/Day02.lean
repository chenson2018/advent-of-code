import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def parse_02 : Parser (Array Int) := many (do let x ← int; ws; pure x)

def safe_01 (xs : List Int) : Bool := 
  let diffs := xs.tail.zipWith (· - ·) xs
  let abs_cond := diffs.all ((· ≤ 3) ∘ Int.natAbs)
  let direction := diffs.all (· < 0) ∨ diffs.all (0 < ·) 
  abs_cond ∧ direction

def List.remove_combo (xs : List α) : List (List α) := (List.range xs.length).map xs.eraseIdx

def safe_02 (xs : List Int) : Bool := xs.remove_combo.any safe_01

@[aoc_main day_02]
def day_02 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let parsed ← lines.mapM (IO.ofExcept ∘ parse_02.run)
  let parsed := parsed.map Array.toList
  
  let p1_ans := parsed.filter safe_01 |> Array.size
  assert! p1_ans = 534
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := parsed.filter safe_02 |> Array.size
  assert! p2_ans = 577
  println! s!"Part 2 answer: {p2_ans}"
