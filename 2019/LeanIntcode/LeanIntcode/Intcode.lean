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
  /-- relative base -/
  base : Int
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
| Add'
| Mult 
| Halt 
| In 
| Out 
| JumpTrue 
| JumpFalse 
| Less 
| Equal 
| Base
deriving Repr, DecidableEq

open Opcode

inductive Mode where 
| Imm 
| Pos 
| Rel
deriving Repr, DecidableEq

open Mode

/-- convert an integer into an opcode -/
def Int.toOpcode? (val : Int) : Option Opcode :=
  match val with
  | 1  => some Add'
  | 2  => some Mult
  | 3  => some In
  | 4  => some Out
  | 5  => some JumpTrue
  | 6  => some JumpFalse
  | 7  => some Less
  | 8  => some Equal
  | 9  => some Base
  | 99 => some Halt
  | _  => none

/-- number of arguments for each opcode -/
def Opcode.len (opcode : Opcode) : Nat :=
  match opcode with
  | Halt                       => 0
  | In | Out | Base            => 1
  | JumpTrue | JumpFalse       => 2
  | Add' | Mult | Equal | Less => 3

/-- mode of the nth argument of an opcode -/
def Int.getMode?  (val: Int) (n : Nat) : Option Mode := 
  match val / 10^(n+1) % 10 with
  | 0 => some Pos
  | 1 => some Imm
  | 2 => some Rel
  | _ => none

namespace Intcode

/-- initialize an Intcode VM with some defaults -/
def new (data : Array Int) (input := @List.nil Int) (silent := false) : Intcode := {
  data := data ++ mkArray (2^15) 0, 
  ptr := 0, 
  halted := False, 
  output := [], 
  input, 
  silent
  base := 0}

/-- execute one opcode -/
def tick (vm : Intcode) : OptionT IO Intcode := do
  -- read the raw opcode
  let raw_opcode ← vm.data.get? vm.ptr

  -- the last two digits are the opcode
  let opcode ← (raw_opcode % 100).toOpcode?

  let interp_read (pos : Nat) : OptionT IO Int := do
    let imm ← vm.data.get? (vm.ptr + pos)
    let mode ← raw_opcode.getMode? pos
    match mode with
    | Imm => some imm
    | Pos => vm.data.get? imm.natAbs
    | Rel => vm.data.get? (imm + vm.base).natAbs

  let interp_write (pos : Nat) : OptionT IO Int := do
    let imm ← vm.data.get? (vm.ptr + pos)
    let mode ← raw_opcode.getMode? pos
    match mode with
    | Imm => none
    | Pos => some imm
    | Rel => some (imm + vm.base)

  -- a utility for the simple opcodes
  let advance := vm.ptr + opcode.len + 1
  let try_set (idx val : Int) (advance := advance) (input := vm.input) := do
    let data ←
      match Nat.decLt idx.natAbs vm.data.size with
        | isTrue h => some (vm.data.set ⟨idx.natAbs, h⟩ val)
        | isFalse _ => none
    pure {vm with ptr := advance, data, input}

  match opcode with
  | Add'  => try_set (← interp_write 3) ((← interp_read 1) + (← interp_read 2))
  | Mult => try_set (← interp_write 3) ((← interp_read 1) * (← interp_read 2))
  | In   => 
      let (val,input) ← match vm.input with
        -- case for precomputed input
        | val :: input => pure (val,input)
        -- otherwise actually read from stdin
        | [] => 
          let stdin ← IO.getStdin
          let input ← stdin.getLine
          let val ← input.trim.toInt?
          pure (val,[])    
      try_set (← interp_write 1) val (input := input)
  | Out  => 
      let out ← interp_read 1
      if vm.silent then pure () else println! s!"Intcode output: {out}"
      pure {vm with ptr := advance, output := out :: vm.output}
  | Halt => pure {vm with halted := True}
  | JumpTrue => 
      let val2 ← interp_read 2
      pure {vm with ptr := if (← interp_read 1) ≠ 0 then val2.natAbs else advance}
  | JumpFalse => 
      let val2 ← interp_read 2
      pure {vm with ptr := if (← interp_read 1) = 0 then val2.natAbs else advance}
  | Less => try_set (← interp_write 3) (if (← interp_read 1) < (← interp_read 2) then 1 else 0)
  | Equal => try_set (← interp_write 3) (if (← interp_read 1) = (← interp_read 2) then 1 else 0)
  | Base => pure {vm with ptr := advance, base := vm.base + (← interp_read 1)}

/-- executes opcodes until VM halts -/
def run := bindUntil Intcode.halted tick

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

