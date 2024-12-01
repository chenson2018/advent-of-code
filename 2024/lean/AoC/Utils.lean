import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

-- from https://github.com/kmill/kmill-aoc2023/blob/e7b994f5985a8414241d0e147ff8541e32e589dc/AoC2023/Util.lean#L18
def Char.digit? (c : Char) : Option Nat :=
  if 48 ≤ c.val && c.val ≤ 57 then
    some (c.val - 48).toNat
  else
    none

def digit' : Parser Nat := do
  let c ← digit
  return (c.val - 48).toNat

def nat : Parser Nat := do
    return (← many1 digit').foldl (init := 0) (fun a d => 10 * a + d)
