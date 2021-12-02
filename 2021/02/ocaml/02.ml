#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* type for the incoming instructions *)

type instruction = 
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

let parse_ins (s: string) : instruction = 
  let parse (n: int) (g: int): string =
    Str.matched_group g s
  in

  let pattern = regexp "\\(.*?\\) \\([0-9]+\\)" in
  let n = Str.search_forward pattern s 0 in

  {direction =               (parse n 1); 
   units     = int_of_string (parse n 2)}

(* these two functions advance a given submarine by a single instruction *)

let p1_interp (state: submarine) (ins: instruction): submarine = 
  match ins with
  | {direction = "forward"; units = x} -> { state with horizontal = state.horizontal + x}
  | {direction = "down";    units = x} -> { state with depth      = state.depth      + x}
  | {direction = "up";      units = x} -> { state with depth      = state.depth      - x}
  | _                                  -> assert false 

let p2_interp (state: submarine) (ins: instruction): submarine = 
  match ins with
  | {direction = "forward"; units = x} -> { state with horizontal = state.horizontal +           x; 
                                                       depth      = state.depth      + state.aim*x}
  | {direction = "down";    units = x} -> { state with aim        = state.aim + x}
  | {direction = "up";      units = x} -> { state with aim        = state.aim - x}
  | _                                  -> assert false 

let () = 
  let input        = In_channel.read_lines "../input.txt" in
  let instructions = List.map ~f:parse_ins input in

  let initial_state = {horizontal = 0; depth = 0; aim = 0} in

  let p1_state = instructions |> List.fold_left ~f:p1_interp ~init:initial_state in
  let p2_state = instructions |> List.fold_left ~f:p2_interp ~init:initial_state in

  printf "Part 1 answer: %d\n" (p1_state.depth*p1_state.horizontal);
  printf "Part 2 answer: %d\n" (p2_state.depth*p2_state.horizontal);
