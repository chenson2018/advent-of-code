#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* take a list of chars, then split it based on the given parsing rules *)
let rec parse (l: char list): char list list  = 
  match l with
  | [] -> []
  | '\\':: 'x' :: hex1 :: hex2 :: tl -> (['\\'; 'x'; hex1; hex2]) :: parse tl
  | one :: two :: tl -> (match one with
                         | '\\' -> ([one; two]) :: parse       tl   (* if an escaped char, take both, *)
                         |  _   -> ([one]     ) :: parse (two::tl)  (* otherwise take just the first  *)
                        )
  | last::[] -> [[last]] (* unmatched char at end of string *)


(* take a list of chars, adding escapes based on encoding rules *)
let rec encode (l: char list): char list  =
  match l with
  | [] -> []
  | one :: two :: tl -> (match one with
                         | '"' | '\\' -> '\\':: one :: encode (two::tl)
                         | _          ->        one :: encode (two::tl)
                        )
  | last::[] -> ['\\'; last]

let () =
  let input       =  In_channel.read_lines "../input.txt" 
                       |> List.map ~f:(String.to_list) in

  let raw_lens    =  List.map ~f:(List.length) input 
                      |> List.fold_left ~f:(+) ~init:0 in

  (* add/subtract 2 for the quotes at begining/end *)
  let parsed_lens =  List.map ~f:(fun i -> List.length (parse  i) - 2) input in
  let encoded_lens = List.map ~f:(fun i -> List.length (encode i) + 2) input in

  let p1_ans      = raw_lens - (List.fold_left ~f:(+) ~init:0 parsed_lens )             in 
  let p2_ans      =            (List.fold_left ~f:(+) ~init:0 encoded_lens) - raw_lens  in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;

