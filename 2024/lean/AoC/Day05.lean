import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std
open Std.HashMap
open Std.HashSet

def Array.middle (xs : Array α) : Option α := xs[xs.size / 2]?

-- this and the below theorem is just for experimentation's sake
def Array.middle' (xs : Array α) : Option α := 
  if h : xs.size = 0 
  then 
    none
  else 
    some $ xs.get ⟨xs.size / 2, Nat.bitwise_rec_lemma h⟩

theorem Array.middle_eq_middle' (xs : Array α) : xs.middle = xs.middle' := by
  let {toList := xs} := xs
  induction xs <;> simp [Array.middle, Array.middle']

namespace Day05

def parse_rule : Parser (Nat × Nat) := do
  let lower ← nat
  skipChar '|'
  let upper ← nat
  pure (lower, upper)

def parse_update : Parser (Array Nat) := many (do let x ← nat; _ ← many (skipChar ','); pure x)

-- given an array of rules, create a hashmap where values must occur after the key
def rules_map (rules : Array (Nat × Nat)) : HashMap Nat (HashSet Nat) := 
  rules.foldl (λ m (lower, upper) => m.alter lower (λ hs => insert (hs.getD empty) upper)) empty

def valid_update? := aux [] where
  aux (prev : List Nat) (map : HashMap Nat (HashSet Nat)) (xs : List Nat) : Bool :=
   match xs with
   | [] => true
   | hd :: tl =>
       let set := map.getD hd empty
       let check_current := prev.all (not ∘ set.contains)
       check_current ∧ aux (hd :: prev) map tl

def rules_qsort (map : HashMap Nat (HashSet Nat)) (xs : Array Nat) : Array Nat := 
    xs.qsort (λ a b => map[a]?.elim false (contains · b))

@[aoc_main day_05]
def day_05 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename
  
  let updates ← text.filter (flip String.contains ',') |>.mapM parse_update.run |> IO.ofExcept
  let rules ← text.filter (flip String.contains '|') |>.mapM parse_rule.run |> IO.ofExcept
  let map := rules_map rules

  let (valid,invalid) := updates.partition (valid_update? map ∘ Array.toList) 

  let p1_ans := valid |>.map (flip Option.getD 0 ∘ Array.middle) |>.foldl (·+·) 0
  assert! p1_ans = 5166
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := invalid |>.map (rules_qsort map) |>.map (flip Option.getD 0 ∘ Array.middle) |>.foldl (·+·) 0
  assert! p2_ans = 4679
  println! s!"Part 2 answer: {p2_ans}"
