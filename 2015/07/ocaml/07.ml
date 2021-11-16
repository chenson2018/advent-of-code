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
  | SIGNAL of signal

(* I was going to just use a regex, but had some issues... will come back to fix *)

let expr_of_string (s: string) : expr =
  if ( string_match ( regexp "^[a-z]+$" ) s 0 ) then
    SIGNAL (Var s)
  else
  if ( string_match ( regexp "^[0-9]+$" ) s 0 ) then
    SIGNAL (Val (int_of_string s))
  else 
    SIGNAL (Var "Not Implemented")

let print_expr (e: expr) : unit = 
  match e with 
  | SIGNAL (Var x) -> printf "VAR: %s\n" x
  | SIGNAL (Val x) -> printf "VAL: %d\n" x
  | _ -> printf "%s" "Not implemented"

(* a record representing an assignment *)

type assignment = 
  {
  lhs: string;
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

  {lhs = lhs_s; 
   rhs = expr_of_string rhs_s;}

let () =
  let input = In_channel.read_lines "../input.txt" in
  let assignments = List.map ~f:parse_assignment input in
    List.iter ~f:(fun a -> print_expr a.rhs) assignments;
