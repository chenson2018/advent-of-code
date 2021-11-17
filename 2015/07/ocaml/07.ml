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

(* an lhs can be a signal or operator *)

type lhs = 
  | Signal of signal
  | Operator of operator

(* simpliy an operator where all signals are know Val int *)

let simplify_op (op: lhs): lhs = 
  match op with
  | Operator {op_lhs = Some (Val l); op = "AND"    ; op_rhs = Val r} -> Signal (Val (l land  r))
  | Operator {op_lhs = Some (Val l); op = "OR"     ; op_rhs = Val r} -> Signal (Val (l lor   r))
  | Operator {op_lhs = Some (Val l); op = "LSHIFT" ; op_rhs = Val r} -> Signal (Val (l lsl   r))
  | Operator {op_lhs = Some (Val l); op = "RSHIFT" ; op_rhs = Val r} -> Signal (Val (l lsr   r))
  | Operator {op_lhs =         None; op = "NOT"    ; op_rhs = Val r} -> Signal (Val (65535 - r))
  | _ -> op 

let print_lhs (l: lhs): unit = 
  match l with
  | Signal (Val x) -> printf "Val: %d\n" x
  | Signal (Var x) -> printf "Var: %s\n" x
  | Operator {op_lhs=_; op=o; op_rhs=_} -> printf "Operator %s called\n" o

(* assignment of an operator to a signal *)

type assignment = 
  {
  l: lhs;
  r: signal; (* should really only be Var of string, but not sure how *)
  }

(* parse lhs *)
let signal_of_string (s: string): signal =
  try
    Val (int_of_string s)
  with
    Failure _ -> Var s

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

(* read input *)

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

(* iterate through assignments an see which are fully resolved *)

let hash_val (ht: (string, int) t) (a: assignment) : unit =
  match a with
  | {l = Signal (Val value); r =  Var key } -> Hashtbl.set ~key:key ~data:value ht
  | _ -> ()

let () = 
  let input = In_channel.read_lines "../input.txt" in
  let assignments = List.map ~f:parse_assignment input in
  let ht = Hashtbl.create (module String) in
    List.iter ~f:(fun a -> hash_val ht a) assignments;
    Hashtbl.iteri ht ~f:(fun ~key ~data -> print_endline (Printf.sprintf "%s->%d" key data));
