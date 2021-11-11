#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* parse directions into tuples *)

let parse (input: string): (int * int) list = 
  let interp_arrow (arrow: char):  (int * int) = 
    match arrow with
    | '>' -> ( 1,  0)
    | '<' -> (-1,  0)
    | '^' -> ( 0,  1) 
    | 'v' -> ( 0, -1)
    | _ -> assert false
  in
  String.to_list input 
  |> List.map ~f:interp_arrow

let add_tuple (l: (int * int)) (r: (int * int)): (int * int) = 
  match l with
  | (x0, y0) -> 
    match r with
    | (x1, y1) -> (x0 + x1, y0 + y1)

(* get intermediate list of visited tuples *)

let cumulative (l: (int * int) list): (int * int) list = 
  let rec aux (l: (int * int) list) (a: (int * int)): (int * int) list =
    match l with
    | [] -> []
    | x::xs -> (add_tuple a x) :: aux xs (add_tuple a x) in
  aux l (0, 0)

(* 
a print for tuples 
not used below but was helpful debugging
could be more general
*)

let print_tuple (tuple: (int * int)): unit = 
  match tuple with
  | (x, y) -> printf "(%d,%d)\n" x y

(* I sort the tuples so that it is easy to remove duplicates *)

let compare_tuple (l: (int * int)) (r: (int * int)): int = 
  match l with
  | (x0, y0) ->
    match r with
    | (x1, y1) -> if (x0 = x1 && y0 = y1) then 0
                  else if (x0 <> x1) then (x0 - x1)
                  else (y0 - y1)

(* 
probably something more general with indicies can be done
note the subtle difference in the pattern matching
*)

let rec get_even = function
| x :: y :: tl -> y :: (get_even tl)
| [] | _::[] -> []

let rec get_odd = function
| x :: y :: tl -> x :: (get_odd tl)
| last::[] -> [last]
| [] -> []

let () =
  let input  = In_channel.read_all "../input.txt" in
  (* make sure to include the starting house *)
  let moves  = parse input in
  let houses = List.dedup_and_sort ~compare:compare_tuple (cumulative ((0, 0)::moves)) in
  let p1_ans = List.length houses in
  (* splits for part 2 *)
  let santa = cumulative ( (0, 0)::(get_odd  moves) ) in
  let robo  = cumulative ( (0, 0)::(get_even moves) ) in
  let combined = List.append santa robo in
  let p2_houses = List.dedup_and_sort ~compare:compare_tuple combined in   
  let p2_ans = List.length p2_houses in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
