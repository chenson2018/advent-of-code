#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* parse each box dimensions into a sorted list *)

let parse_lines (lines: string list):  int list list = 
  let parse line = 
    String.split_on_chars ~on:[ 'x' ] line
    |> List.map ~f:int_of_string
    |> List.sort ~compare:Int.compare
  in
  List.map ~f:parse lines

(* calculation for part one *)

let paper_needed dims = 
  match dims with
  | s::m::l::_ -> 3*s*m + 2*s*l + 2*m*l
  | _       -> assert false

(* calculation for part two *)

let ribbon_needed dims = 
  match dims with
  | s::m::l::_ -> 2*(s+m) + (List.fold_left ~f:( * ) ~init:1 dims)
  | _       -> assert false

let () =
  let input  = In_channel.read_lines "../input.txt" in
  let parsed = parse_lines input in
  let paper  = List.map ~f:paper_needed parsed in
  let ribon  = List.map ~f:ribbon_needed parsed in
  let p1_ans = List.fold_left ~f:(+) ~init:0 paper in
  let p2_ans = List.fold_left ~f:(+) ~init:0 ribon in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
