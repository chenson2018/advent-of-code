import LeanIntcode.Aux

/-- A structure representing an Intcode virtual machine -/
structure Intcode where
  /-- an array of integer-valued memory -/
  data : Array Int
  /-- the current instruction pointer -/
  ptr : Nat
  /-- is the Vm halted? -/
  halted : Bool
  /-- a record of any outputs made (in reverse order) -/
  output : List Int
  /-- values for precomputed input -/
  input : List Int
  /-- option to supress printing -/
  silent : Bool
deriving Repr, Inhabited

/-- Iterate a monad until some condtion is met -/
partial def bindUntil [Monad m] (cond : A → Bool) (iter : A → m A) (a : A) : m A :=
  if cond a then pure a else iter a >>= bindUntil cond iter

/-- lift any optional type into OptionT -/
def hoistOption [Pure m] : Option T → OptionT m T := OptionT.mk ∘ pure

-- see https://leanprover.zulipchat.com/#narrow/channel/217875-Is-there-code-for-X.3F/topic/Equivalent.20to.20Haskell's.20hoistMaybe
instance {m} [Pure m] : MonadLift Option (OptionT m) where
  monadLift := hoistOption

inductive Opcode where 
| Add 
| Mult 
| Halt 
| In 
| Out 
| JumpTrue 
| JumpFalse 
| Less 
| Equal 
deriving Repr, DecidableEq

inductive Mode where 
| Imm 
| Pos 
deriving Repr, DecidableEq

open Opcode in
/-- convert an integer into an opcode -/
def Int.toOpcode? (val : Int) : Option Opcode :=
  match val with
  | 1  => some Add
  | 2  => some Mult
  | 3  => some In
  | 4  => some Out
  | 5  => some JumpTrue
  | 6  => some JumpFalse
  | 7  => some Less
  | 8  => some Equal
  | 99 => some Halt
  | _  => none

/-- number of arguments for each opcode -/
def Opcode.len (opcode : Opcode) : Nat :=
  match opcode with
  | Halt                      => 0
  | In | Out                  => 1
  | JumpTrue | JumpFalse      => 2
  | Add | Mult | Equal | Less => 3

/-- mode of the nth argument of an opcode -/
def Int.getMode  (val: Int) (n : Nat) := if 0 = val / 10^(n+2) % 10 then Mode.Pos else Mode.Imm

namespace Intcode

/-- initialize an Intcode VM with some defaults -/
def new (data : Array Int) (input := @List.nil Int) (silent := false) : Intcode := {data, ptr := 0, halted := False, output := [], input, silent}

/-- execute one opcode -/
def tick (vm : Intcode) : OptionT IO Intcode := do
  -- read the raw opcode
  let raw_opcode ← vm.data.get? vm.ptr

  -- the last two digits are the opcode
  let opcode ← (raw_opcode % 100).toOpcode?

  -- get the possible immediate values
  -- TODO: I use zero as a default value since this is eagerly evaluated, is there a better way?
  let imm1 ← if 1 ≤ opcode.len then (vm.data.get? (vm.ptr + 1)) else some 0
  let imm2 ← if 2 ≤ opcode.len then (vm.data.get? (vm.ptr + 2)) else some 0
  let imm3 ← if 3 ≤ opcode.len then (vm.data.get? (vm.ptr + 3)) else some 0
 
  -- TODO: better conversion here than .natAbs ?
  let val1 ← hoistOption (if raw_opcode.getMode 0 = Mode.Imm then imm1 else if 1 ≤ opcode.len then (vm.data.get? imm1.natAbs) else some 0)
  let val2 ← hoistOption (if raw_opcode.getMode 1 = Mode.Imm then imm2 else if 2 ≤ opcode.len then (vm.data.get? imm2.natAbs) else some 0)

  -- a utility for the simple opcodes
  let advance := vm.ptr + opcode.len + 1
  let try_set (idx val : Int) (advance := advance) (input := vm.input) := do
    let data ←
      match Nat.decLt idx.natAbs vm.data.size with
        | isTrue h => some (vm.data.set ⟨idx.natAbs, h⟩ val)
        | isFalse _ => none
    pure {vm with ptr := advance, data, input}

  match opcode with
  | Opcode.Add  => try_set imm3 (val1 + val2)
  | Opcode.Mult => try_set imm3 (val1 * val2)
  | Opcode.In   => 
      let (val,input) ← match vm.input with
        -- case for precomputed input
        | val :: input => pure (val,input)
        -- otherwise actually read from stdin
        | [] => 
          let stdin ← IO.getStdin
          let input ← stdin.getLine
          let val ← input.trim.toInt?
          pure (val,[])    
      try_set imm1 val (input := input)
  | Opcode.Out  => 
      if vm.silent then pure () else println! s!"Intcode output: {val1}"
      pure {vm with ptr := advance, output := val1 :: vm.output}
  | Opcode.Halt => pure {vm with halted := True}
  | Opcode.JumpTrue => pure {vm with ptr := if val1 ≠ 0 then val2.natAbs else advance}
  | Opcode.JumpFalse => pure {vm with ptr := if val1 = 0 then val2.natAbs else advance}
  | Opcode.Less => try_set imm3 (if val1 < val2 then 1 else 0)
  | Opcode.Equal => try_set imm3 (if val1 = val2 then 1 else 0)

/-- executes opcodes until VM halts -/
def run := bindUntil Intcode.halted tick

open Opcode in 
partial def run_until_output (vm : Intcode) : OptionT IO (Intcode × Int) := do
  let raw_opcode ← vm.data.get? vm.ptr
  let opcode ← (raw_opcode % 100).toOpcode?
  let vm ← vm.tick
  if opcode = Halt ∨ opcode = Out
  then
    match vm.output with
    | out :: _ => pure (vm,out)
    | []       => none
  else
    run_until_output vm

end Intcode

