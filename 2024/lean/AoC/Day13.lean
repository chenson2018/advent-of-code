-- import Mathlib.Algebra.EuclideanDomain.Defs
-- import Mathlib.Algebra.EuclideanDomain.Int
-- import Mathlib.Data.Int.Range
import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

def List.triples (xs : List α) := 
  match xs with
  | a :: b :: c :: tl => (a,b,c) :: tl.triples
  | _ => []

namespace Day13

def parse_button : Parser (Int × Int) := do
  skipString "Button "
  skipChar 'A' <|> skipChar 'B'
  skipString ": X+"
  let x ← nat
  skipString ", Y+"
  let y ← nat
  pure (x, y)

def parse_prize : Parser (Int × Int) := do
  skipString "Prize: X="
  let x ← nat
  skipString ", Y="
  let y ← nat
  pure (x,y)

def Int.div_exact (a b : Int) : Option Int := 
  if a % b = 0 then some (a / b) else none

-- an integer system of equations:
-- ax + by = p
-- cx + dy = q
def solve (a b p c d q : Int) := do
  let y_top := c*p - a*q
  let y_bot := c*b - a*d
  let y ← Int.div_exact y_top y_bot
  let x ← Int.div_exact (p - b*y) a
  pure (x,y)

def score (xs : List (Option (Int × Int))) := xs.reduceOption.map (λ (x,y) ↦ 3*x+y) |>.sum

@[aoc_main day_13]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let input ← text.filter (·.length != 0) |>.mapM (parse_button <|> parse_prize).run |> IO.ofExcept
  let input : List ((Int × Int) × (Int × Int) × (Int × Int)) := input.toList.triples

  let p1_ans := input.map (λ ((a,c),(b,d),(p,q)) ↦ solve a b p c d q) |> score
  assert! p1_ans = 25751
  println! s!"Part 1 answer: {p1_ans}"

  let offset := 10000000000000
  let p2_ans := input.map (λ ((a,c),(b,d),(p,q)) ↦ solve a b (offset+p) c d (offset+q)) |> score
  assert! p2_ans = 108528956728655
  println! s!"Part 2 answer: {p2_ans}"
