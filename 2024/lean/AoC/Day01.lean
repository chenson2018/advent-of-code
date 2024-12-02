import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def parse_01 : Parser (Nat × Nat) := do
  let a ← nat
  _ ← ws
  let b ← nat
  pure (a, b)

-- array to counts of elements
def Array.counts {α : Type} [BEq α] [Hashable α] (xs : Array α) : Std.HashMap α Nat := 
  xs.foldl (λ m a => m.insert a $ m.getD a 0 + 1) Std.HashMap.empty

open Prod in
@[aoc_main day_01]
def day_01 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let parsed ← lines.mapM (IO.ofExcept ∘ parse_01.run)
  
  let (l,r) := parsed.unzip
  let l := (l.map Int.ofNat).qsort (Ordering.isLT $ compare · ·)
  let r := (r.map Int.ofNat).qsort (Ordering.isLT $ compare · ·)
  
  let diffs := Array.zipWith l r (Int.natAbs $ · - ·)
  let p1_ans := diffs.foldl (· + ·) 0
  assert! p1_ans = 1151792
  println! s!"Part 1 answer: {p1_ans}"
  
  let scores := l.map (λ x => x * r.counts.getD x 0)
  let p2_ans := scores.foldl (· + ·) 0
  assert! p2_ans = 21790168
  println! s!"Part 2 answer: {p2_ans}"
