#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* type for the incoming instructions *)

type ins = 
  {
  direction: string;
  units: int;
  }

(* type for the state of the submarine *)

type submarine = 
  {
  horizontal: int;
  depth: int;
  aim: int;
  }

(* parse a string to an instruction *)

let parse_ins (s: string) : ins = 
  let parse (n: int) (g: int): string =
    Str.matched_group g s
  in

  let pattern = regexp "\\(.*?\\) \\([0-9]+\\)" in
  let n = Str.search_forward pattern s 0 in

  {direction =               (parse n 1); 
   units     = int_of_string (parse n 2)}



let () = 
  let input        = In_channel.read_lines "../input.txt" in
  let instructions = List.map ~f:parse_ins input in

  let initial_state = {horizontal = 0; depth = 0; aim = 0} in

  let p1_end = List.fold_left ~f:(fun ({horizontal = h; depth = d; aim = a} as state) ins -> 
               match ins with
               | {direction = "forward"; units = x} -> { state with horizontal = h + x}
               | {direction = "down";    units = x} -> { state with depth      = d + x}
               | {direction = "up";      units = x} -> { state with depth      = d - x}
               | _                                  -> assert false 
               )
               ~init:initial_state
               instructions
  in

  let p2_end = List.fold_left ~f:(fun ({horizontal = h; depth = d; aim = a} as state) ins -> 
               match ins with
               | {direction = "forward"; units = x} -> { state with horizontal = h + x; depth = d + a*x}
               | {direction = "down";    units = x} -> { state with aim        = a + x}
               | {direction = "up";      units = x} -> { state with aim        = a - x}
               | _                                  -> assert false 
               ) 
               ~init:initial_state 
               instructions
  in

  printf "Part 1 answer: %d\n" (p1_end.depth*p1_end.horizontal);
  printf "Part 2 answer: %d\n" (p2_end.depth*p2_end.horizontal);
