import AoC.CmdAttr
import AoC.Utils
open Std.Internal.Parsec.String
open Std.Internal.Parsec

structure RotateDial where
  cc? : Bool
  ticks : Nat
deriving Repr

def RotateDial.toInt (dial : RotateDial) : Int := 
  (if dial.cc? then -1 else 1) * dial.ticks

def parse_01 : Parser RotateDial := do
  let dir ← pchar 'L' <|> pchar 'R'
  let cc? := dir == 'L'
  let ticks ← nat
  return {cc?, ticks}

def p1 (xs : Array RotateDial) (start := 50) (n :=  100) : Nat := Id.run do
  let mut pos : Int := start
  let mut res := 0
  for dial in xs do
    pos := pos + dial.toInt
    if pos % n = 0 then res := res + 1
  return res

@[aoc_main day_01]
def day_01 (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let lines ← IO.FS.lines filename
  let parsed ← lines.mapM (IO.ofExcept ∘ parse_01.run)

  let p1_ans := p1 parsed
  --let p2_ans := p2 parsed
  println! s!"Part 1 answer: {p1_ans}"
  --println! s!"Part 2 answer: {p2_ans}"
