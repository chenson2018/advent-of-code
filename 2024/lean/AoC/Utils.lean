import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

-- this is in a recent PR to Std
namespace Std.HashMap
  variable {α : Type u} {β : Type v} {_ : BEq α} {_ : Hashable α}

  def alter (m : HashMap α β) (a : α) (f : Option β → Option β) : HashMap α β :=
    match m.get? a with
    | none =>
      match f none with
      | none => m
      | some b => m.insert a b
    | some b =>
      match f (some b) with
      | none => m.erase a
      | some b => m.erase a |>.insert a b
end Std.HashMap

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

def int : Parser Int := do
  let n ← nat
  pure (Int.ofNat n)
