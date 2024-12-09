import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

def List.tryInsertMany (xs : List α) (f : α → Option (List α)) : Option (List α) :=
  match xs with
  | [] => none
  | hd :: tl =>
      match f hd with
      | some ys => some (ys ++ tl)
      | none => do hd :: (← tl.tryInsertMany f)

namespace Day09

structure Contiguous where
  id : Option Nat
  length : Nat
deriving Repr

def List.toContiguous := aux 0 where
  aux (id : Nat) (xs : List Nat) : List Contiguous :=
    match xs with
    | [] => []
    | [length] => [{id, length}]
    | l_files :: l_space :: tl => {id, length := l_files} :: {id := none, length := l_space} :: aux (id + 1) tl

partial def p1_calc (front back : List Contiguous) (acc limit : Nat) : List Contiguous := 
  -- the last entry usually needs to be cut off, do it here so we don't have to repeat traversal
  let trunc x := if limit ≤ acc + x then limit - acc else x

  if acc < limit then
    match front, back with
    -- shouldn't reach these...
    | [], _ | _, [] => []
    -- throw out empty blocks
    | {length := 0, ..} :: ftl, back => p1_calc ftl back acc limit
    | front, {length := 0, ..} :: btl => p1_calc front btl acc limit
    -- throw out spaces at the back
    | front, {id := none, ..} :: btl => p1_calc front btl acc limit
    -- pass along files at the front
    | front@{id := some _, ..} :: ftl, back =>
       let t_len := trunc front.length
      {front with length := t_len} :: p1_calc ftl back (acc+t_len) limit
    -- fill spaces in front with back
    | front@{id := none, length := f_len} :: ftl, back@{id := some _, length := b_len} :: btl => 
        if f_len ≤ b_len then
          let t_len := trunc f_len
          {id := back.id, length := t_len} :: p1_calc ftl ({back with length := b_len - f_len} :: btl) (acc+t_len) limit
        else
          let t_len := trunc b_len
          {id := back.id, length := t_len} :: p1_calc ({front with length := f_len - b_len} :: ftl) btl (acc+t_len) limit
  else
    []

def p1 (xs : List Contiguous) : List Contiguous :=
  let n_elems := xs.map (λ c => if c.id.isSome then c.length else 0) |>.foldl (·+·) 0
  p1_calc xs xs.reverse 0 n_elems

def checksum (xs : List Contiguous) : Nat :=
  let ids := xs.map (flip Option.getD 0 ∘ Contiguous.id)
  let lengths := xs.map Contiguous.length
  let id_rep := lengths.zipWith List.replicate ids |>.join
  let check := id_rep.foldl (λ (acc,pos) id => (acc + pos * id,pos+1)) (0,0)
  check.fst

def Contiguous.merge (l r : Contiguous) : Option (List Contiguous) := 
  match l, r with
  | {id := some _, length := l_len}, {id := none, length := r_len} =>
      if l_len ≤ r_len then
        some $ [l, {id := none, length := r_len - l_len}].filter ((·>0) ∘ Contiguous.length)
      else 
        none
  | _, _ => none

partial def p2 (xs : List Contiguous) : List Contiguous := 
  match xs with 
  | [] => []
  | last :: front_rev =>
      match front_rev.reverse.tryInsertMany (Contiguous.merge last) with
      | none => last :: p2 front_rev
      | some new_front => {last with id := none} :: p2 new_front.reverse

@[aoc_main day_09]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename 
  let nums ← Parser.run (many digit') text |> IO.ofExcept
  let nums := nums.toList

  let conts := List.toContiguous nums |>.filter ((·>0) ∘ Contiguous.length)

  let p1_ans := p1 conts |> checksum
  assert! p1_ans = 6415184586041
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := (p2 conts.reverse).reverse |> checksum
  assert! p2_ans = 6436819084274 
  println! s!"Part 2 answer: {p2_ans}"
