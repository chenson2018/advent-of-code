#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* type reresenting an individual light and its instruction *)

type light_action = 
  | Toggle | On | Off

type light = 
  {
  x: int;
  y: int;
  action: light_action;
  }

(* calculate on/off given a state and a record with an action *)

let calc_action state record = 
  match record with
  | {x = _; y = _; action = Toggle } -> not state
  | {x = _; y = _; action = On     } -> true
  | {x = _; y = _; action = Off    } -> false

(* type representing an instruction for a given rectangle *)

type instruction = 
  {
  action: light_action;
  x0: int;
  y0: int;
  x1: int;
  y1: int;
  }

(* convert a string into the light_action type *)

let action_of_string (s: string) : light_action =
  match s with
  | "off" -> Off
  | "on"  -> On
  | "toggle" -> Toggle
  | _ -> assert false


(* take a string and convert into a record *)

let parse_instruction (s: string) : instruction = 
  let parse (n: int) (g: int): string = 
    Str.matched_group g s 
  in
  (* I'm using the OCaml regex for now, put it's pretty awful compared to pcre *)
  let pattern = regexp ".*?\\(toggle\\|off\\|on\\) \\([0-9]+\\),\\([0-9]+\\) through \\([0-9]+\\),\\([0-9]+\\)" in
  let n = Str.search_forward pattern s 0 in
  {action = action_of_string (parse n 1); 
   x0 = int_of_string (parse n 2); 
   y0 = int_of_string (parse n 3); 
   x1 = int_of_string (parse n 4); 
   y1 = int_of_string (parse n 5)}
  

(* 
next steps:
  - for each calculate the tuples affected and have a big list of light records
  - for each possible tuple, filter the big list and fold with calc_action
*)

let () =
  let input = In_channel.read_lines "../input.txt" in
  let instructions = List.map ~f:parse_instruction input in
  let test = List.find_exn ~f:(fun ins -> match ins with | {action = Toggle} -> true | {action = _ } -> false) instructions in
    printf "%d\n" (List.length instructions);
    printf "%d\n" test.x1;
