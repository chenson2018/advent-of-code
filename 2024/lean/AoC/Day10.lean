import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod

def Array.withIdx (grid : Array (Array α)) : HashMap (Int × Int) α := 
  let range_x := grid.size
  let range_y := grid[0]!.size
  let idx := product (range range_x |>.map Int.ofNat)  (range range_y |>.map Int.ofNat)
  let with_idx := idx.zip grid.flatten
  Std.HashMap.ofList with_idx.toList

namespace Day10

def get_adj (map : HashMap (Int × Int) α) (point : Int × Int) : List ((Int × Int) × α):= 
  let (x,y) := point
  let adj := [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]
  adj.map (λ idx => do (idx, ← map[idx]?)) |>.reduceOption
  
def get_paths (map : HashMap (Int × Int) Nat) (height : Nat) (acc : List (Int × Int)) :=
  let adj := acc.map (get_adj map) |>.flatten |>.filter ((·==height) ∘ snd) |>.map fst
  if height = 0 then adj else get_paths map (height - 1) adj

def all_paths (map : HashMap (Int × Int) Nat) : List (List (Int × Int)) :=
  map
  |>.filter (λ _ x => x = 9) |>.keys
  |>.map (get_paths map 8 [·])

@[aoc_main day_10]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let nums ← text.mapM (Parser.run (many digit')) |> IO.ofExcept

  let map := Array.withIdx nums

  let p1_ans := all_paths map |>.map (Std.HashSet.size ∘ Std.HashSet.ofList) |>.sum
  assert! p1_ans = 535
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := all_paths map |>.map (·.length) |>.sum
  assert! p2_ans = 1186
  println! s!"Part 2 answer: {p2_ans}"  
