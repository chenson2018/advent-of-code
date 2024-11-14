import LeanIntcode.Tactic

structure Intcode where
  data : Array Nat
  ptr : Nat
  halted : Bool
deriving Repr

partial def bindUntil [Monad m] (cond : A → Bool) (iter : A → m A) (a : A) : m A :=
  if cond a then pure a else iter a >>= bindUntil cond iter

inductive Opcode where | Add | Mult | Halt deriving Repr

open Opcode in
def Nat.toOpcode? (val : Nat) : Option Opcode :=
  match val with
  | 1  => some Add
  | 2  => some Mult
  | 99 => some Halt
  | _  => none

def Opcode.len (opcode : Opcode) : Nat :=
  match opcode with
  | Add | Mult => 3
  | Halt       => 0

namespace Intcode

def new (data : Array Nat) : Intcode := {data, ptr := 0, halted := False}

def hoistOption [Monad m] : Option T → OptionT m T := OptionT.mk ∘ pure

-- TODO: setting values can still fail here

def tick (vm : Intcode) : OptionT IO Intcode := do
  let raw_opcode ← hoistOption (vm.data.get? vm.ptr)
  let opcode ← hoistOption raw_opcode.toOpcode?
  -- TODO: I use zero as a default value since this is eagerly evaluated, is there a better way?
  let imm1 ← hoistOption (if 1 ≤ opcode.len then (vm.data.get? (vm.ptr + 1)) else some 0)
  let imm2 ← hoistOption (if 2 ≤ opcode.len then (vm.data.get? (vm.ptr + 2)) else some 0)
  let imm3 ← hoistOption (if 3 ≤ opcode.len then (vm.data.get? (vm.ptr + 3)) else some 0)
  let pos1 ← hoistOption (if 1 ≤ opcode.len then (vm.data.get? imm1) else some 0)
  let pos2 ← hoistOption (if 2 ≤ opcode.len then (vm.data.get? imm2) else some 0)
  let pos3 ← hoistOption (if 3 ≤ opcode.len then (vm.data.get? imm3) else some 0)
  match opcode with
  | Opcode.Add  => pure {vm with data := vm.data.set! imm3 (pos1 + pos2), ptr := vm.ptr + opcode.len + 1}
  | Opcode.Mult => pure {vm with data := vm.data.set! imm3 (pos1 * pos2), ptr := vm.ptr + opcode.len + 1}
  | Opcode.Halt => pure {vm with halted := True}

def run := bindUntil Intcode.halted tick

end Intcode

