import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

-- modified from mathlib
def Nat.log10 (n : Nat) : Nat := if h : 10 ≤ n  then log10 (n / 10) + 1 else 0

-- memoization is a monad!
abbrev Memo (α β : Type) [BEq α] [Hashable α]:= StateM (HashMap α β) β

def memo [BEq α] [Hashable α] (a : α) (compute : Memo α β) : Memo α β := do
  match (← get)[a]? with
  | some b => pure b
  | none =>
      let b ← compute
      modify (·.insert a b)
      pure b

def List.map_memo [BEq α] [Hashable α] (xs : List α) (f : α → Memo α β) (m : HashMap α β) : List β × HashMap α β :=
    xs.foldl (λ (acc,m) a ↦ let (b,m) := f a |>.run m; (b :: acc, m)) ([],m)

namespace Day11

partial def blink_memo (depth stone : Nat) : Memo (Nat × Nat) Nat := memo (depth,stone) (uncurry aux (depth,stone)) where
  aux (d s : Nat) :=
      if d = 0 then
        pure 1
      else if s = 0 then
        blink_memo (d - 1) 1
      else 
        let n_digits := Nat.log10 s + 1
      if n_digits % 2 = 0 then
        let front := s / 10^(n_digits / 2)
        let back  := s % 10^(n_digits / 2)
        (·+·) <$> blink_memo (d - 1) front <*> blink_memo (d - 1) back
      else
        blink_memo (d - 1) (s * 2024)

@[aoc_main day_11]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename 
  let input ← text.splitOn " " |>.mapM nat.run  |> IO.ofExcept

  -- we can reuse part 1 for part 2!!!
  let (p1_counts,cache) := input.map (25,·) |>.map_memo (uncurry blink_memo) HashMap.empty
  let (p2_counts,_)     := input.map (75,·) |>.map_memo (uncurry blink_memo) cache

  let p1_ans := p1_counts.sum
  assert! p1_ans = 190865
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := p2_counts.sum
  assert! p2_ans = 225404711855335
  println! s!"Part 2 answer: {p2_ans}"
