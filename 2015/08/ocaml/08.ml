#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

let parse (s: string): char list list =
  let l = String.to_list s in
  let rec aux (l: char list)  = 
    match l with
    | [] -> []
    | '\\':: 'x' :: hex1 :: hex2 :: tl -> (['\\'; 'x'; hex1; hex2]) :: aux tl
    | one :: two :: tl -> (match one, two with
                           | '\\', _ -> ([one; two]) :: aux       tl 
                           |  _  , _ -> ([one]     ) :: aux (two::tl)
                          )
    | last::[] -> [[last]] 
  in
  aux l

let () =
  let input       = In_channel.read_lines "../input.txt" in
  let raw_lens    = List.map ~f:(String.length) input in
  (* subtracting 2 for the quotes at begining/end *)
  let parsed_lens = List.map ~f:(fun i -> List.length (parse i) - 2) input in
  let p1_ans      = (List.fold_left ~f:(+) ~init:0 raw_lens) - (List.fold_left ~f:(+) ~init:0 parsed_lens) in
    printf "Part 1 answer: %d\n" p1_ans;
(*    List.iter ~f:(printf "%d\n") parsed_lens;*)
(*  let str = "\\x27" in
  let len = String.length str in
  let p   = List.length (parse str) in
    printf "Diff: %d" (len - p);*)
(*    List.iter ~f:(fun i -> (printf "%s" "["; List.iter ~f:(printf "%c,") i); print_endline "]") input*)
