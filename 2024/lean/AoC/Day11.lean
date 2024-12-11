import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

-- modified from mathlib
def Nat.log10 (n : Nat) : Nat := if h : 10 ≤ n  then log10 (n / 10) + 1 else 0

namespace Day11

def blink (n : Nat) : List Nat :=
  if n = 0 then
    [1]
  else 
    let n_digits := Nat.log10 n + 1
  if n_digits % 2 = 0 then
    let front := n / 10^(n_digits / 2)
    let back  := n % 10^(n_digits / 2)
    [front, back]
  else
    [n * 2024]

@[aoc_main day_11]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename 
  let input ← text.splitOn " " |>.mapM nat.run  |> IO.ofExcept

  let p1_ans := (·.flatMap blink)^[25] input |>.length 
  assert! p1_ans = 190865
  println! s!"Part 1 answer: {p1_ans}"
