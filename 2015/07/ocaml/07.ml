#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str
open Hashtbl

(* a variable or integer *)
type signal = 
  | Var of string
  | Val of int

(* an operator on the lhs *)
type operator = 
  {
  op_lhs: signal option;
  op: string;
  op_rhs: signal;
  } 

(* a lhs can be a signal or operator *)
type lhs = 
  | Signal of signal
  | Operator of operator

(* simplify an operator where all signals are known Val int *)
let simplify_op (op: lhs): lhs = 
  match op with
  | Operator {op_lhs = Some (Val l); op = "AND"    ; op_rhs = Val r} -> Signal (Val (l land  r))
  | Operator {op_lhs = Some (Val l); op = "OR"     ; op_rhs = Val r} -> Signal (Val (l lor   r))
  | Operator {op_lhs = Some (Val l); op = "LSHIFT" ; op_rhs = Val r} -> Signal (Val (l lsl   r))
  | Operator {op_lhs = Some (Val l); op = "RSHIFT" ; op_rhs = Val r} -> Signal (Val (l lsr   r))
  | Operator {op_lhs =         None; op = "NOT"    ; op_rhs = Val r} -> Signal (Val (65535 - r))
  | _ -> op 

(* 
  assignment of an operator to a signal 
  r should only be a Var String, but I had trouble with types,
  but note that this is enfored in the parser function however 
*)
type assignment = 
  {
  l: lhs;
  r: signal;
  }

(* I'd prefer to use regex but having trouble with matches being reset *)
let signal_of_string (s: string): signal =
  try
    Val (int_of_string s)
  with
    Failure _ -> Var s

(* parse lhs *)
let lhs_of_string (s: string) : lhs = 
  if ( string_match ( regexp "^[a-z0-9]+$" ) s 0 ) then
    Signal (signal_of_string s) else

  if ( string_match ( regexp "^NOT \\([a-z0-9]+\\)$" ) s 0 ) then
    Operator {op_lhs = None; 
              op = "NOT"; 
              op_rhs = (signal_of_string (Str.matched_group 1 s));} 
  else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) AND \\([a-z0-9]+\\)$" ) s 0 ) then
    Operator {op_lhs = Some ( signal_of_string (Str.matched_group 1 s) );
              op = "AND"; 
              op_rhs = (signal_of_string (Str.matched_group 2 s));} 
  else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) OR \\([a-z0-9]+\\)$" ) s 0 ) then
    Operator {op_lhs = Some ( signal_of_string (Str.matched_group 1 s) );
              op = "OR"; 
              op_rhs = (signal_of_string (Str.matched_group 2 s));} 
  else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) LSHIFT \\([a-z0-9]+\\)$" ) s 0 ) then
    Operator {op_lhs = Some ( signal_of_string (Str.matched_group 1 s) );
              op = "LSHIFT"; 
              op_rhs = (signal_of_string (Str.matched_group 2 s));} 
  else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) RSHIFT \\([a-z0-9]+\\)$" ) s 0 ) then
    Operator {op_lhs = Some ( signal_of_string (Str.matched_group 1 s) );
              op = "RSHIFT"; 
              op_rhs = (signal_of_string (Str.matched_group 2 s));} 
  else
    assert false

(* parse full input lines *)
let parse_assignment (s: string) : assignment = 
  let parse (n: int) (g: int): string =
    Str.matched_group g s
  in
  let pattern = regexp "\\(.*?\\) -> \\(.*\\)" in
  let n = Str.search_forward pattern s 0 in

  let lhs_s = parse n 1 in
  let rhs_s = parse n 2 in

  {l = lhs_of_string lhs_s;
   r = Var rhs_s;}

(* add fully resolved assignments to a hash *)
let hash_val (ht: (string, int) t) (a: assignment) : unit =
  match a with
  | {l = Signal (Val value); r =  Var key } -> Hashtbl.set ~key:key ~data:value ht
  | _ -> ()


(* 
  one iteration of simplification this takes an assignment then

  1) replaces values already in the hash
  2) then simplifies lhs if possible
*)
let replace_hash (ht: (string, int) t ) (a: assignment) : assignment = 
  match a.l with
  | Operator {op_lhs = op_lhs'; op = op'; op_rhs = Var var} -> 
    (match (Hashtbl.find ht var) with
     | Some x -> {l = simplify_op (Operator {op_lhs = op_lhs'; op = op'; op_rhs = Val x} ) ; r = a.r}
     | None   -> a)
  | Operator {op_lhs = Some (Var var); op = op'; op_rhs = op_rhs'} -> 
    (match (Hashtbl.find ht var) with
     | Some x -> {l = simplify_op (Operator {op_lhs = Some (Val x); op = op'; op_rhs = op_rhs'}) ; r = a.r}
     | None   -> a)
  | Signal (Var var) -> 
    (match (Hashtbl.find ht var) with
     | Some x -> {l = simplify_op (Signal (Val x)) ; r = a.r}
     | None   -> a)
  | _ -> a

let advance_state (ht: (string, int) t ) (al: assignment list) : assignment list = 
    List.iter ~f:(fun a -> hash_val ht a) al;
    List.map ~f:(fun a -> (replace_hash ht a) ) al

let rec find_wire (ht: (string, int) t ) (al: assignment list) (find: string): int = 
  match (Hashtbl.find ht find) with
  | Some x -> x
  | None   -> find_wire ht (advance_state ht al) find

let () = 
  let input = In_channel.read_lines "../input.txt" in 
  let target_wire = "a" in
  (* 
    parse the assignments
    create a hash for storing resolved wires
    simplify until the has contains the desired key 
  *)
  let assignments_p1 = List.map ~f:parse_assignment input in
  let ht_p1 = Hashtbl.create (module String) in
  let p1_ans = find_wire ht_p1 assignments_p1 target_wire in
    printf "Part 1 answer: %d\n" p1_ans;

  (* now for part 2, same problem but change the value of a wire *)
  (* hung when I tried to use a variable for "b" in a pattern match, should inverstigate later *)
  let ht_p2 = Hashtbl.create (module String) in
  let assignments_p2 =  ({l = Signal (Val p1_ans); r = Var "b"})
                      ::(List.filter ~f:(fun a -> 
                                          match a.r with 
                                          | Var "b" -> false 
                                          | _       -> true )
                         assignments_p1) 
  in
  let p2_ans = find_wire ht_p2 assignments_p2 target_wire in
    printf "Part 2 answer: %d\n" p2_ans;

(* nice way to print hashes *)
(* Hashtbl.iteri ht ~f:(fun ~key ~data -> print_endline (Printf.sprintf "%s->%d" key data)); *)

