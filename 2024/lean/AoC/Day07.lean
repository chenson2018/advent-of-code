import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

namespace Day07

structure Calibration where
  lhs : Nat
  rhs : List Nat
deriving Repr, Inhabited

def parse : Parser Calibration := do
  let lhs ← nat
  skipString ":"
  let rhs ← many (ws *> nat)
  pure {lhs, rhs := rhs.toList}

def Calibration.eval (cal : Calibration) (ops : List (Nat → Nat → Nat)) : Nat := 
  match cal.rhs with
  | [] => 0
  | init :: nums => ops.zipWith (flip · $ ·) nums |>.foldl (flip (· $ ·)) init

def Calibration.combos (cal : Calibration) (ops : List (Nat → Nat → Nat)) : List Nat :=
  match cal.rhs with
  | [] => []
  | init :: tl => aux init ops tl
  where  
    aux (acc : Nat) (ops : List (Nat → Nat → Nat)) (nums : List Nat) := 
      match nums with
      | [] => [acc]
      | hd :: tl => ops.map (· acc hd) |>.map (aux . ops tl) |>.join

def Calibration.valid (ops : List (Nat → Nat → Nat)) (cal : Calibration) : Bool :=
  cal.lhs ∈ cal.combos ops

-- modified from mathlib
def Nat.log (b : Nat) : Nat → Nat
  | n =>
    if h : b ≤ n ∧ 1 < b then
      have : n / b < n := Nat.div_lt_self (by omega) (by omega)
      log b (n / b) + 1
    else 0

def Nat.concat (l r : Nat) : Nat := 
  l * 10^(Nat.log 10 r + 1) + r

@[aoc_main day_07]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let calibrations ← text.mapM parse.run |> IO.ofExcept

  let (p1_valid,p1_invalid) := calibrations.partition (Calibration.valid [Nat.add, Nat.mul])
  let p1_ans := p1_valid.map Calibration.lhs |>.foldl (·+·) 0
  assert! p1_ans = 2501605301465
  println! s!"Part 1 answer: {p1_ans}"

  let p2_valid := p1_invalid.filter (Calibration.valid [Nat.add, Nat.mul, Nat.concat])
  let p2_ans := p1_ans + (p2_valid.map Calibration.lhs |>.foldl (·+·) 0)
  assert! p2_ans = 44841372855953
  println! s!"Part 2 answer: {p2_ans}"
