import AoC.CmdAttr
import AoC.Utils
import Batteries.Data.Array.Basic
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std Prod Std.HashMap Function

namespace Day14

structure Robot where
  px : Int
  py : Int
  vx : Int
  vy : Int
deriving Repr

def parse_robot : Parser Robot := do
  skipString "p="
  let px ← int
  skipChar ','
  let py ← int
  ws
  skipString "v="
  let vx ← int
  skipChar ','
  let vy ← int
  pure {px,py,vx,vy}

def Robot.move (r : Robot) (xlim ylim s : Nat) : Robot :=
  let px := (r.px + r.vx * s) % xlim
  let py := (r.py + r.vy * s) % ylim
  {r with px, py}

def safety_factor (robots : Array Robot) (xlim ylim s : Nat) := 
  let moved := robots.map (·.move xlim ylim s)
  let xmid : Int := xlim / 2
  let ymid : Int := ylim / 2
  let q1 := moved.filter (λ r ↦ r.px > xmid ∧ r.py < ymid) |>.size  
  let q2 := moved.filter (λ r ↦ r.px < xmid ∧ r.py < ymid) |>.size
  let q3 := moved.filter (λ r ↦ r.px < xmid ∧ r.py > ymid) |>.size
  let q4 := moved.filter (λ r ↦ r.px > xmid ∧ r.py > ymid) |>.size
  q1 * q2 * q3 * q4 

def print_robots (robots : Array Robot) (xlim ylim : Nat) : IO Unit := do
    let coor := robots.map (λ r ↦ (r.px.toNat,r.py.toNat))
    for y in [0:ylim] do
      for x in [0:xlim] do
        if (x,y) ∈ coor
        then 
          IO.print "⬜"
        else
          IO.print "⬛"
      println! ""

-- my guess was that the tree would have a lot of robots in a row
-- arrangements can only repeat at max xlim*ylim
def lines (robots : Array Robot) := 
  let x_idx := robots.map Robot.px |>.toList
  let x_max := x_idx.max?.getD 0 |>.toNat
  let x_line := List.range x_max |>.map (λ x ↦ x_idx.count ↑x)
  let y_idx := robots.map Robot.py |>.toList
  let y_max := y_idx.max?.getD 0 |>.toNat
  let y_line := List.range y_max |>.map (λ y ↦ y_idx.count ↑y)
  x_line.max?.getD 0 + y_line.max?.getD 0

def p2 (robots : Array Robot) (xlim ylim : Nat) :=
  let times := List.range (xlim * ylim) |> Array.mk
  let frames := times.map (λ t ↦ (t,robots.map (·.move xlim ylim t)))
  frames.getMax? (λ (_,xs) (_,ys) ↦ lines xs < lines ys) |>.get!

@[aoc_main day_14]
def main (args : List String) : IO Unit := do
  let [filename] := args | throw <| IO.userError "Expecting one argument, the input file"
  let text ← IO.FS.lines filename 
  let robots ← text.mapM parse_robot.run |> IO.ofExcept

  let xlim := 101
  let ylim := 103

  let p1_ans := safety_factor robots xlim ylim 100
  assert! p1_ans = 219150360
  println! s!"Part 1 answer: {p1_ans}"

  let (p2_ans,tree) := p2 robots xlim ylim
  assert! p2_ans = 8053
  println! s!"Part 2 answer: {p2_ans}"
  print_robots tree xlim ylim
