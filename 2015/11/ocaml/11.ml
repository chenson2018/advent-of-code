#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* this type stores the result of incrementing a single character *)
type inc_res = 
  | Carry of char
  | Res   of char

(* increment a character, looping z -> Carry a *)
let inc_char (c: char): inc_res = 
  match c with
  | 'z' -> Carry 'a'
  |  _  -> Res ( Char.of_int_exn ( (Char.to_int c) + 1 ) )

let from_res (res: inc_res): char = 
  match res with
  | Res x -> x
  | _     -> assert false

(* e.g. abc *)
let increasing_straight (xs: char list): bool = 
  let rec aux (xs: char list): bool list = 
    match xs with
    | 'y' :: tl                      (* exclude straights that wrap around back to 'a' *)
    | 'z' :: tl -> false :: (aux tl) 
    | one :: two :: three :: tl -> (
                                    (Char.equal two   (one |> inc_char |> from_res)) &&
                                    (Char.equal three (two |> inc_char |> from_res))
                                   ) :: aux (two :: three :: tl)
    | _ -> [false]
  in
  List.mem (aux xs) true ~equal:(Bool.equal)

let valid_chars (xs: char list): bool = 
  not ( 
        List.mem xs 'i' ~equal:(Char.equal) ||
        List.mem xs 'o' ~equal:(Char.equal) ||
        List.mem xs 'l' ~equal:(Char.equal)
      )

(* e.g. aabb, note that aaa does not count *)
let two_pair (xs: char list): bool = 
  let rec aux (xs: char list): int list = 
    match xs with
    |  one :: two :: tl when Char.equal one two ->  1 :: aux tl
    |  one :: two :: tl                         ->  0 :: aux (two::tl)
    |  _                                        -> [0]
  in
  let total = List.fold_left ~f:(+) (aux xs) ~init:0 in
  total >= 2

(* the three rules combined *)
let valid_password (xs: char list): bool = 
  increasing_straight xs &&
  valid_chars         xs &&
  two_pair            xs

(* increment a password a single iteration, including the carry *)
let inc_password (xs: char list): char list = 
 (* apply an increment to the rightmost character *)
 (* I reverse the list here so it is easier to recurse over *)
  let next = (match List.rev xs with
              | last :: tl -> (inc_char last) :: List.map ~f:(fun x -> Res x) tl
              | _          -> assert false)
  in

  (* now resolve any carries, including cascading ones *)
  let rec aux (xs: inc_res list): char list = 
    match xs with
    | Carry one :: Res two :: tl -> one :: aux ((inc_char two)::tl)
    |              Res one :: tl -> one :: aux                  tl
    | [] -> []
    | _  -> assert false
  in
  List.rev (aux next)

(* increment until we have a valid password *)
let rec next_passwd (xs: char list): char list = 
  let next = inc_password xs in
  if (valid_password next) then next else next_passwd next

let () =
  let input  = ['h'; 'e'; 'p'; 'x'; 'c'; 'r'; 'r'; 'q'] in
  let p1_ans = next_passwd input  in
  let p2_ans = next_passwd p1_ans in
    printf "Part 1 answer: %s\n" (String.of_char_list p1_ans);
    printf "Part 2 answer: %s\n" (String.of_char_list p2_ans);
