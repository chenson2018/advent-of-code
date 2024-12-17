import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.Array.Basic
import Batteries.Data.List.Basic
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

structure Computer where
  A : Nat
  B : Nat
  C : Nat
  ptr : Nat
  program : Array (Fin 8)
  out : List Nat
deriving Repr

inductive Interp | Literal | Combo deriving Repr, DecidableEq

open Interp

def Computer.interp (c : Computer) (interp : Interp) (operand : Fin 8) : Option Nat :=
  if interp = Literal then
    some operand
  else
    match operand with
    | 0 | 1 | 2 | 3 => some operand
    | 4 => c.A
    | 5 => c.B
    | 6 => c.C
    | 7 => none

def opcode_interp (opcode : Fin 8) : Interp :=
  match opcode with
  | 0 | 2 | 5 | 6 | 7 => Combo
  | 1 | 3 | 4 => Literal

def Computer.tick (c : Computer) : Option Computer := do
  let opcode      ← c.program[c.ptr]?
  let raw_operand ← c.program[c.ptr+1]?
  let operand     ← c.interp (opcode_interp opcode) raw_operand
  let ptr := c.ptr + 2
  pure $
  match opcode with
  | 0 => {c with A := c.A / 2^operand, ptr}
  | 1 => {c with B := c.B ^^^ operand, ptr}
  | 2 => {c with B := operand % 8, ptr}
  | 3 => if c.A = 0 then {c with ptr} else {c with ptr := operand}
  | 4 => {c with B := c.B ^^^ c.C, ptr}
  | 5 => {c with out := operand % 8 :: c.out, ptr}
  | 6 => {c with B := c.A / 2^operand, ptr}
  | 7 => {c with C := c.A / 2^operand, ptr}

partial def bindAll (iter : α → Option α) (a : α) : α :=
  match iter a with
  | none => a
  | some next => bindAll iter next

def Computer.repr_out (c : Computer) : String := ",".intercalate $ c.out.reverse.map Nat.repr

namespace Day17

def parse : Parser Computer := do
  let A ← skipString "Register A: " *> nat
  let B ← ws *> skipString "Register B: " *> nat
  let C ← ws *> skipString "Register C: " *> nat
  let program ← ws *>  skipString "Program: " *> many ((many (skipChar ',')) *> fin 8)
  pure {A, B, C, program, ptr := 0, out := []}

@[aoc_main day_17]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.readFile filename 
  let computer ← parse.run text |> IO.ofExcept
  
  let p1_ans := (bindAll Computer.tick computer).repr_out
  assert! p1_ans = "7,1,3,4,1,2,6,7,1"
  println! s!"Part 1 answer: {p1_ans}"
