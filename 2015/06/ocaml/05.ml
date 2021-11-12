#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* 
variant reresenting an action 
could just use a string in this problem, but trying to be idiomatic
*)
type light_action = 
  | Toggle | On | Off

(* record representing an instruction for a given rectangle *)
type instruction = 
  {
  action: light_action;
  x0: int;
  y0: int;
  x1: int;
  y1: int;
  }

(* calculate new state given a state and a record with an action *)
let calc_action_p1 (state: bool) (record: instruction) : bool = 
  match record with
  | { x0=_; y0=_; x1=_; y1=_; action = Toggle } -> not state
  | { x0=_; y0=_; x1=_; y1=_; action = On     } -> true
  | { x0=_; y0=_; x1=_; y1=_; action = Off    } -> false

let calc_action_p2 (state: int) (record: instruction) : int = 
  match record with
  | { x0=_; y0=_; x1=_; y1=_; action = Toggle } -> state + 2
  | { x0=_; y0=_; x1=_; y1=_; action = On     } -> state + 1
  | { x0=_; y0=_; x1=_; y1=_; action = Off    } -> max 0 (state - 1)

(* convert a string into the light_action type *)
let action_of_string (s: string) : light_action =
  match s with
  | "off" -> Off
  | "on"  -> On
  | "toggle" -> Toggle
  | _ -> assert false


(* take a string and convert into an instruction record *)
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
from https://stackoverflow.com/questions/10893521/how-to-take-product-of-two-list-in-ocaml
I think this is not tail recursive
*)
let cartesian l l' = 
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e,e')) l') l)

(* from https://stackoverflow.com/questions/243864/what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)
let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

(* this returns a boolean indicating if an instruction affects a particuliar record *)
let ins_includes (tup: (int * int) ) (ins: instruction): bool = 
  match tup with
  | (x, y) -> (ins.x0 <= x) && (x <= ins.x1) && (ins.y0 <= y) && (y <= ins.y1)

(*
The progrssion is

list of instruction records ->
map each grid point to a filtered list of instructions that affect it, type instruction list list ->
for each problem, map this lists of lists to a fold to get the light state, so int list ->
get the final summation

*)

let () =
  let input = In_channel.read_lines "../input.txt" in
  let grid = cartesian (0--999) (0--999) in
  let instructions = List.map ~f:parse_instruction input in

  let light_l = List.map ~f:(fun g -> 
                  List.filter ~f:(fun ins -> 
                    ins_includes g ins 
                  ) instructions
                ) grid 
  in 

  let states_p1 = List.map ~f:(fun l -> 
                    Bool.to_int ( List.fold_left ~f:calc_action_p1 ~init:false l )
                  ) light_l 
  in

  let states_p2 = List.map ~f:(fun l -> 
                    List.fold_left ~f:calc_action_p2 ~init:0 l
                  ) light_l 
  in

  let p1_ans  = List.fold_left ~f:(+) ~init:0 states_p1 in
  let p2_ans  = List.fold_left ~f:(+) ~init:0 states_p2 in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
