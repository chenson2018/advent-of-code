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

def int : Parser Int := do
  let n ← nat
  pure (Int.ofNat n)

-- from Mathlib
namespace List
  variable {α β : Type}
  variable (r : α → α → Prop) [DecidableRel r] {l : List α} {o : Option α} {a : α}
  variable [LT β] [@DecidableRel β (· < ·)] {f : α → β} {l : List α} {a m : α}

  def argAux (a : Option α) (b : α) : Option α :=
    Option.casesOn a (some b) fun c => if r b c then some b else some c

  def argmax (f : α → β) (l : List α) : Option α :=
    l.foldl (argAux fun b c => f c < f b) none

  def argmin (f : α → β) (l : List α) :=
    l.foldl (argAux fun b c => f b < f c) none
end List

def Array.product (l₁ : Array α) (l₂ : Array β) : Array (α × β) := 
  l₁.flatMap fun a => l₂.map (Prod.mk a)
