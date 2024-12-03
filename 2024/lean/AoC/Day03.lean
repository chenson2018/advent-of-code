import AoC.CmdAttr
import AoC.Utils
import Regex
open Std.Internal.Parsec.String
open Std.Internal.Parsec

namespace Day03

inductive Instruction 
| Enable
| Disable
| Mult : Nat → Nat → Instruction
deriving Repr, DecidableEq

open Instruction

open Parser in
def Instruction.parse : Parser Instruction := 
  (do 
    optSkipString "do"
    (do _ ← optSkipString "()"; pure Enable) <|> 
    (do _ ← optSkipString "n't()"; pure Disable)
  ) 
  <|>
  (do
    _ ← optSkipString "mul("
    let a ← nat
    _ ← optSkipString ","
    let b ← nat
    _ ← optSkipString ")"
    pure $ Mult a b
  )

def extract_parse (text : String) :=
  let re := regex% r"(mul\(\d+,\d+\)|do\(\)|don't\(\))"
  let captures := Regex.all_captures text re
  let groups := flip Option.getD #[] $ captures.mapM (Array.mapM id ∘ Regex.Captures.groups)
  let raw := groups.flatten.mapM (Instruction.parse.run ∘ Substring.toString)
  raw

def p1_interp (ins : Instruction) : Nat := 
  match ins with
  | Mult a b => a * b
  | _ => 0

def p2_fold (state : Nat × Bool) (ins : Instruction) : Nat × Bool := 
  match state, ins with
  | (total,_), Enable      => (total,true)
  | (total,_), Disable     => (total,false)
  | (_,false), Mult _ _    => state
  | (total,true), Mult a b => (total + a * b,true)

@[aoc_main day_03]
def day_03 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename
  let instructions ← IO.ofExcept (extract_parse text)

  let p1_ans := (instructions.map p1_interp).foldl (· + ·) 0
  assert! p1_ans = 191183308
  println! s!"Part 1 answer: {p1_ans}"

  let p2_ans := instructions.foldl p2_fold (0,true) |> Prod.fst
  assert! p2_ans = 92082041
  println! s!"Part 2 answer: {p2_ans}"  
