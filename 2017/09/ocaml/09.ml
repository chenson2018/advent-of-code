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
                  |  _  -> one :: remove_garbage                tl
              )

(* all of the above applied *)
let parse (xs: char list): char list = 
  xs 
  |> remove_exclamation
  |> remove_garbage 

(* 
this logic took a bit to figure out
every time we encounter a new opening '{' we increment and add that to our list
after we match it, we deincrment
we'll be left with a list where the numbers correspond to matching brackets at that depth
if we add up and divide by two, this is the score we are asked for
this function assumes that we have only '{', '}', and ',' in a well-formed pattern
*)
let score (xs: char list) = 
  let rec aux (xs: char list) (inc: int): int list = 
    match xs with
    | [] -> []
    | '{' :: '{' :: tl -> (inc + 1) :: aux ('{'::tl) (inc + 1)
    | '{' :: '}' :: tl -> (inc    ) :: aux ('}'::tl) (inc    )
    | '}' :: '{' :: tl -> (inc    ) :: aux ('{'::tl) (inc    )
    | '}' :: '}' :: tl -> (inc - 1) :: aux ('}'::tl) (inc - 1)
    | one :: ',' :: tl -> 0         :: aux (one::tl) (inc    )
    | last::[] -> [1] (* count for the very first '{' *)
    | _ -> assert false
  in
  let counts = List.fold_left ~f:(+) ~init:0 (aux xs 1) in
  counts / 2
  
let () =
  let input = In_channel.read_all "../input.txt" 
                |> String.strip 
                |> String.to_list
  in

  let parsed  = parse input  in 
  let p1_ans =  score parsed in
     printf "Part 1 answer: %d\n" p1_ans;
