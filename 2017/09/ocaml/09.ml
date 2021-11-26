#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* e.g.  {!x} -> {} *)
let rec remove_exclamation (xs: char list): char list  =
  match xs with
  | [] -> []
  | one :: two :: tl -> (match one with
                         | '!'        ->        remove_exclamation       tl
                         | _          -> one :: remove_exclamation (two::tl)
                        )
  | last::[] -> [last]


(* e.g. abc?def -> def *)
let rec past_garbage (xs: char list): char list = 
  match xs with
  | [] -> assert false
  | '>' :: tl ->              tl
  | _   :: tl -> past_garbage tl

(* eg {<x>} -> {} *)
let rec remove_garbage (xs: char list): char list  =
  match xs with
  | [] -> []
  | one :: tl -> (match one with
               | '<' ->        remove_garbage ( past_garbage tl )
               | _   -> one :: remove_garbage                tl
              )

(* e.g. {,,,} -> {} *)
let rec remove_extra_comma (xs: char list): char list = 
  match xs with
  | '{' :: ',' :: tl -> remove_extra_comma ('{' :: tl)
  | ',' :: '}' :: tl -> remove_extra_comma ('}' :: tl)
  | ',' :: ',' :: tl -> remove_extra_comma (       tl)
  | one :: two :: tl -> one :: two :: (remove_extra_comma tl)
  | last::[] -> [last]
  | [] -> []

(* all of the above applied *)
let parse (xs: char list): char list = 
  xs 
  |> remove_exclamation
  |> remove_garbage 
  |> remove_extra_comma


(* 
WIP on scoring 
currently only works if there are no commas
e.g. {} ->1, {{}} -> 3, etc.
*)
let rec score (xs: char list) (count: int): int list = 
  match xs with
  | [] -> []
  | '{' :: '{' :: tl -> (count + 1) :: score ('{'::tl) (count + 1)
  | '{' :: '}' :: tl -> (count    ) :: score ('}'::tl) (count    )
  | '}' :: '{' :: tl -> (count    ) :: score ('{'::tl) (count    )
  | '}' :: '}' :: tl -> (count - 1) :: score ('}'::tl) (count - 1)
  | one :: ',' :: tl -> 0           :: score (one::tl) (count    )
  | ',' :: one :: tl -> 0           :: score (one::tl) (count    )
  | last::[] -> [1]
  
let () =
  let input   = In_channel.read_all "../input.txt" |> String.strip |> String.to_list in
  (*let input   = "{{{},{},{{}}}}" |> String.to_list in*)
  let parsed  = parse input in 

    print_endline "Original:";
    List.iter ~f:(printf "%c") input;
    print_endline "";
    print_endline "";

    print_endline "Parsed:";
    List.iter ~f:(printf "%c") parsed;
    print_endline "";


  let p1_ans = score parsed 1 in
     List.iter ~f:(printf "%d ") p1_ans;
     print_endline "";
     printf "%d\n" ((List.fold_left ~f:(+) ~init:0 p1_ans) / 2)
