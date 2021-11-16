#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* 
this type represents a singal
can be either an integer or a variable
raises an error for invalid integer signal
*)

type signal = 
  | Var of string
  | Val of int

type expr = 
  | NOT of signal
  | AND of signal * signal
  | OR of signal * signal
  | LSHIFT of signal * signal
  | RSHIFT of signal * signal
  | SIGNAL of signal

let signal_of_string s = 
  try
    Val (int_of_string s)
  with
    Failure _ -> Var s
(*
when doing two regex the match is reset!!!

  if ( string_match ( regexp "^[a-z]+$" ) s 0 ) then
    Var s
  else
  if ( string_match ( regexp "^[0-9]+$" ) s 0 ) then
    Val (int_of_string s)
  else
    assert false
*)
  
let string_of_signal (si: signal) : string = 
  match si with 
  | Var x -> x
  | Val x -> string_of_int x

let expr_of_string (s: string) : expr = 
  if ( string_match ( regexp "^[a-z0-9]+$" ) s 0 ) then
    SIGNAL (signal_of_string s) else 

  if ( string_match ( regexp "^NOT \\([a-z0-9]+\\)$" ) s 0 ) then
    NOT (signal_of_string ( Str.matched_group 1 s ) ) else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) AND \\([a-z0-9]+\\)$" ) s 0 ) then
    AND (signal_of_string (Str.matched_group 1 s),
         signal_of_string (Str.matched_group 2 s)) else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) OR \\([a-z0-9]+\\)$" ) s 0 ) then
    OR (signal_of_string (Str.matched_group 1 s),
        signal_of_string (Str.matched_group 2 s)) else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) LSHIFT \\([a-z0-9]+\\)$" ) s 0 ) then
    LSHIFT (signal_of_string (Str.matched_group 1 s),
            signal_of_string (Str.matched_group 2 s)) else

  if ( string_match ( regexp "^\\([a-z0-9]+\\) RSHIFT \\([a-z0-9]+\\)$" ) s 0 ) then
    RSHIFT (signal_of_string (Str.matched_group 1 s), 
            signal_of_string (Str.matched_group 2 s))

  else
    assert false

let print_expr (e: expr) : unit = 
  match e with
  | SIGNAL s -> printf "SIGNAL: %s\n" (string_of_signal s)
  | NOT s -> printf "NOT %s\n" (string_of_signal s)
  | AND (s, s') -> printf "%s AND %s\n" (string_of_signal s) (string_of_signal s')
  | OR (s, s') -> printf "%s OR %s\n" (string_of_signal s) (string_of_signal s')
  | LSHIFT (s, s') -> printf "%s LSHIFT %s\n" (string_of_signal s) (string_of_signal s')
  | RSHIFT (s, s') -> printf "%s RSHIFT %s\n" (string_of_signal s) (string_of_signal s')

(* a record representing an assignment *)

type assignment = 
  {
  lhs: expr;
  rhs: expr;
  }

let parse_assignment (s: string) : assignment = 
  let parse (n: int) (g: int): string = 
    Str.matched_group g s 
  in
  (* I'm using the OCaml regex for now, put it's pretty awful compared to pcre *)
  let pattern = regexp "\\(.*?\\) -> \\(.*\\)" in
  let n = Str.search_forward pattern s 0 in

  let lhs_s = parse n 1 in
  let rhs_s = parse n 2 in

  {lhs = expr_of_string lhs_s; 
   rhs = expr_of_string rhs_s;}

let () =
  let input = In_channel.read_lines "../input.txt" in
  let assignments = List.map ~f:parse_assignment input in
  let l1_assignments = List.filter ~f:(fun a -> match a with | {lhs = SIGNAL (Val _); rhs = _ } -> true | _ -> false) assignments in
    List.iter ~f:(fun a -> print_expr a.lhs) l1_assignments;
