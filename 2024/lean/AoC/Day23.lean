import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.Array.Basic
import Batteries.Data.List.Basic
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

namespace Day23

def parse : Parser (String × String) := do
  let l ← take 2
  skipChar '-'
  let r ← take 2
  pure (l,r)

def build_map (xs : Array (String × String)) : HashMap String (List String) := 
  xs.foldl 
  (λ m (k,v) ↦ 
    m.alter k (λ val ↦ 
      match val with
      | none => [v]
      | some vs => some (v :: vs)
    )
  ) 
  HashMap.empty

def iter (m : HashMap String (List String)) (xs : List (List String)) :=
  xs.map 
  (λ ys ↦ 
    match ys with
    | [] => []
    | hd :: _ => (m.get! hd).map (·::ys)
  ) 
  |>.flatten

def p1 (m : HashMap String (List String)) := 
    m.keys.map ([·]) 
    |> iter m 
    |> iter m 
    |> iter m 
    |>.filter (λ xs ↦ xs[0]! == xs.reverse[0]!) 
    |>.map List.tail
    |>.map List.mergeSort 
    |> List.eraseDup 
    |>.filter (λ xs ↦ xs.any (λ s ↦ s.startsWith "t")) 
    |>.length

@[aoc_main day_23]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let pairs ← text.mapM parse.run |> IO.ofExcept
  let m := build_map (pairs ++ pairs.map Prod.swap)

  let p1_ans := p1 m
  assert! p1_ans = 1485
  println! s!"Part 1 answer: {p1_ans}"
