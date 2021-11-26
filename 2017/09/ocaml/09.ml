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
                         | '!' ->        remove_exclamation       tl
                         |  _  -> one :: remove_exclamation (two::tl)
                        )
  | last::[] -> [last]

(* e.g. abc>def -> def *)
let rec past_garbage (xs: char list): char list = 
  match xs with
  | [] -> assert false
  | '>' :: tl ->              tl
  | _   :: tl -> past_garbage tl

(* eg {<x>} -> {<>} *)
let rec remove_garbage (xs: char list): char list  =
  match xs with
  | [] -> []
  | one :: tl -> (match one with
                  | '<' -> '<' :: '>' :: remove_garbage ( past_garbage tl )
                  |  _  -> one ::        remove_garbage                tl
                 )

(* 
this logic took a bit to figure out
we construct a list where the number is the depth level of the corresponding bracket
if we add up and divide by two, this is the score we are asked for
this function assumes that we have only '{', '}', ',', and "<>" (removed garbage marker) in a well-formed pattern
*)
let score (xs: char list) = 
  let rec aux (xs: char list) (inc: int): int list = 
    match xs with
    | '{' :: '{' :: tl -> (inc + 1) :: aux ('{'::tl) (inc + 1)  (* new opening bracket at deeper level        *)
    | '}' :: '}' :: tl -> (inc - 1) :: aux ('}'::tl) (inc - 1)  (* close a bracket and finish that depth      *)
    | '{' :: '}' :: tl -> (inc    ) :: aux ('}'::tl) (inc    )  (* close a bracket and continue at that depth *)
    | '}' :: '{' :: tl -> (inc    ) :: aux ('{'::tl) (inc    )  (* new opening bracket at same depth          *)
    | one ::  _  :: tl -> 0         :: aux (one::tl) (inc    )  (* move past comma or garbage marker          *)
    | last::[] -> [1]                                           (* count for the very first '{'               *)
    | [] -> []
  in
  let counts = List.fold_left ~f:(+) ~init:0 (aux xs 1) in
  counts / 2
  
let () =
  let input = In_channel.read_all "../input.txt"
                |> String.strip 
                |> String.to_list
  in

  let p1_ans =  input 
                |> remove_exclamation 
                |> remove_garbage
                |> score
  in

  let p2_ans = (input |> remove_exclamation                   |> List.length) - 
               (input |> remove_exclamation |> remove_garbage |> List.length) 
  in
     
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
