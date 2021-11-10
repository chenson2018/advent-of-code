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

(* 
I did this myself! Well... mostly by myself. I converted a Haskell function into this.
This is a common pattern, nesting a "helper" function inside our final function.

An example of how this progesses:

   aux [1; 2; 3] 0
-> (0+1) :: aux [2; 3] (0+1) 
-> 1     :: (1+2) :: aux [3] (1+2)
-> 1     :: 3     :: (1+2+3) :: aux [] (1+2+3)
-> 1     :: 3     :: 6
*)

let cumulative (l: int list) : int list = 
  let rec aux (l: int list) (a: int) : int list = 
    match l with
    | [] -> []
    | x::xs -> (a+x) :: aux xs (a+x) in
  aux l 0

(* 
see https://stackoverflow.com/questions/22404124/returning-an-index-of-an-element-in-ocaml 
Note the similarity to the above, where we use an intermediate variable to "accumulate" the index
*)

let index (v: 'a) (l: 'a list): int option =
  let rec aux c = function
    | [] -> None
    | h::t ->
      if h = v then Some c
      else aux (c+1) t
  in 
  aux 0 l

let () =
  let input   = In_channel.read_all "../input.txt" in
  let actions = actions_list input in
  let p1_ans  = List.fold_left ~f:(+) ~init:0 actions in
  (* part 1 could also  take the last element of the following *)
  let action_l  = cumulative actions in
  let p2_ans    = index (-1) action_l in
    printf "Part 1 answer: %d\n" p1_ans;
    match p2_ans with
    | None   -> assert false
    | Some x -> printf "Part 2 answer: %d\n" (x+1);
