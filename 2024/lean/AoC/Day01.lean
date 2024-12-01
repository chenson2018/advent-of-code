import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def parse : Parser (Nat × Nat) := do
  let a ← nat
  _ ← ws
  let b ← nat
  pure (a, b)

open Prod Int in
@[aoc_main day_01_p1]
def part1 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let parsed ← lines.mapM (IO.ofExcept ∘ parse.run)
  let l := (parsed.map (ofNat ∘ fst)).qsort (Ordering.isLT $ compare · ·)
  let r := (parsed.map (ofNat ∘ snd)).qsort (Ordering.isLT $ compare · ·)
  let diffs := Array.zipWith l r (natAbs $ · - ·)
  let ans := diffs.foldl (· + ·) 0
  assert! ans = 1151792
  println! s!"Part 1 answer: {ans}"

-- array to counts of elements
def Array.counts {α : Type} [BEq α] [Hashable α] (xs : Array α) : Std.HashMap α Nat := 
  xs.foldl (fun m a => m.insert a (m.getD a 0 + 1)) Std.HashMap.empty

open Prod Int in
@[aoc_main day_01_p2]
def part2 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let parsed ← lines.mapM (IO.ofExcept ∘ parse.run)
  let l := parsed.map fst
  let r := parsed.map snd
  let r_counts := r.counts
  let scores := l.map (fun x => x * r_counts.getD x 0)
  let ans := scores.foldl (· + ·) 0
  assert! ans = 21790168
  println! s!"Part 2 answer: {ans}"
