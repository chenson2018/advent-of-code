#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* process a single '(' or ')' *)

let interp_paren (paren: char) : int = 
    match paren with
      | '('->  1
      | ')'-> -1
      | _ -> assert false

(* map a string to a list of 1/-1 *)

let actions_list (line: string) : int list = 
  String.to_list line 
    |> List.map ~f:interp_paren

let () =
  let input   = In_channel.read_all "../input.txt" in
  let actions = actions_list input in
  let p1_ans  = List.fold_left ~f:(+) ~init:0 actions in
    printf "Part 1 answer: %d\n" p1_ans;
