#require "core.top"
#require "ppx_deriving.show"

open Core
open Core.String
open Core.Int
open Core.Set
open Core.Fn

module Card = struct
  type card = { id: int; winning: int list; numbers: int list} [@@deriving show] 
end

open Card

let parse input : card = 
  let int_parse xs = int_of_string (of_char_list xs) in
  (* this is fun, note how the first case swaps arguments *)
  let rec aux input xs ys = 
    match input with
    | ' '::'|'::tl -> aux tl ys xs
    | ' '::' '::n1::tl -> aux tl (int_parse [n1    ] :: xs) ys
    | ' '::n1 ::n2::tl -> aux tl (int_parse [n1; n2] :: xs) ys
    | _ -> (xs,ys)
  in
  (* I would rather do this than regex o7 *)
  let (id,body) = match input with
  | 'C'::'a'::'r'::'d'::' '::' '::' '::n1::':'::tl -> (int_parse [n1;       ], tl)
  | 'C'::'a'::'r'::'d'::' '::' ':: n1::n2::':'::tl -> (int_parse [n1; n2    ], tl)
  | 'C'::'a'::'r'::'d'::' ':: n1:: n2::n3::':'::tl -> (int_parse [n1; n2; n3], tl)
  | _ -> assert false
  in
  let (numbers,winning) = aux body [] [] in
  {id; winning; numbers}

let n_intersect card : int = 
  inter (Set.of_list card.winning) (Set.of_list card.numbers) |> Set.length

let score card : int = 
  let count = card |> n_intersect in
  if count = 0 then 0 else pow 2 (count - 1)

(* get a list of n copies of a *)
let rec repeat a n = if n = 0 then [] else a :: (repeat a (n - 1))

let p2_calc cards = 
  let matches = cards |> List.map ~f:(n_intersect) in
  let copies = repeat 1 (List.length cards) in
  let rec aux m c = 
    match m, c with
    | n_matches :: m_tl, n_copies :: c_tl -> (
      let ext = repeat n_copies n_matches in
      let (hd, tl) = List.split_n c_tl n_matches in
      let new_copies = (List.map2_exn ext hd ~f:(+)) @ tl in
      n_copies + (aux m_tl new_copies)
    )
    | _ -> 0
  in
  aux matches copies

let () =
  let cards = In_channel.read_lines "../input.txt" |> List.map ~f:(compose parse String.to_list) in
  let p1_ans = cards |> List.map ~f:score |> List.fold_left ~f:(+) ~init:0 in
  let p2_ans = p2_calc cards in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
