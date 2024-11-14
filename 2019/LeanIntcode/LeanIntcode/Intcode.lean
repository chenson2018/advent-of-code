import LeanIntcode.Tactic

structure Intcode where
  data : Array Nat
  ptr : Nat
  halted : Bool
deriving Repr

partial def bindUntil [Monad m] (cond : A → Bool) (iter : A → m A) (a : A) : m A :=
  if cond a then pure a else iter a >>= bindUntil cond iter

namespace Intcode

def new (data : Array Nat) : Intcode := {data, ptr := 0, halted := False}

-- TODO : possible to convert to non-! versions?
-- TODO : nice way to do args?

def tick (vm : Intcode) : IO Intcode := do
  let opcode := Array.get! vm.data vm.ptr
  match opcode with
  | 1  => 
    let arg1 := Array.get! vm.data (vm.ptr + 1)
    let arg2 := Array.get! vm.data (vm.ptr + 2)
    let arg3 := Array.get! vm.data (vm.ptr + 3)
    pure {vm with data := vm.data.set! arg3 (vm.data.get! arg1 + vm.data.get! arg2), ptr := vm.ptr + 4}
  | 2  => 
    let arg1 := Array.get! vm.data (vm.ptr + 1)
    let arg2 := Array.get! vm.data (vm.ptr + 2)
    let arg3 := Array.get! vm.data (vm.ptr + 3)
    pure {vm with data := vm.data.set! arg3 (vm.data.get! arg1 * vm.data.get! arg2), ptr := vm.ptr + 4}
  | 99 => pure {vm with halted := True}
  | _  => pure vm

def run := bindUntil Intcode.halted tick

end Intcode

