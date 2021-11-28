#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

type inc_res = 
  | Carry of char
  | Res   of char

let inc_char (c: char): inc_res = 
  match c with
  | 'z' -> Carry('a')
  |  _  -> Res ( Char.of_int_exn ( (Char.to_int c) + 1 ) )

let from_res (res: inc_res): char = 
  match res with
  | Res x -> x
  | _     -> assert false

let increasing_straight (xs: char list): bool = 
  let rec aux (xs: char list): bool list = 
    match xs with
    | 'y' :: tl -> false :: (aux tl) (* exclude straights that wrap around back to 'a' *) 
    | 'z' :: tl -> false :: (aux tl) 
    | one :: two :: three :: tl -> ((Char.equal two (from_res (inc_char one))) && (Char.equal three (from_res (inc_char two)))) :: aux (two :: three :: tl)
    | _ -> [false]
  in
  List.mem (aux xs) true ~equal:(Bool.equal)

let invalid_char (xs: char list): bool = 
  List.mem xs 'i' ~equal:(Char.equal) ||
  List.mem xs 'o' ~equal:(Char.equal) ||
  List.mem xs 'l' ~equal:(Char.equal)

let two_pair (xs: char list): bool = 
  let rec aux (xs: char list): int list = 
    match xs with
    |  one :: two :: tl when Char.equal one two -> 1 :: aux tl
    |  one :: two :: tl                         -> 0 :: aux (two::tl)
    |  _                                        -> [0]
  in
  let total = List.fold_left ~f:(+) (aux xs) ~init:0 in
  total >= 2

let valid_password (xs: char list): bool = 
  increasing_straight xs &&
  not (invalid_char xs)  &&
  two_pair xs

let inc_password (xs: char list): inc_res list = 
  match List.rev xs with
  | last :: tl ->  List.rev (inc_char last :: (List.map ~f:(fun x -> Res x) tl))
  | _          -> assert false

let print_inc_res (res: inc_res): unit = 
  match res with
  | Carry x -> printf "[Carry %c]" x
  | Res   x -> printf "[Res %c]" x

let resolve_carry (xs: inc_res list): char list = 
  let rec aux xs = 
    match List.rev xs with
    | (Carry one) :: (Res two) :: tl -> one :: aux (List.rev ((inc_char two)::tl))
    | Res x     :: tl        -> x :: aux (List.rev tl)
    | [] -> []
    | _  -> assert false
  in
  List.rev (aux xs)


let () =
  let ex = ['x'; 'z'; 'z'; 'z'; 'z'; 'z'; 'z'; 'z'] in
  let inc = inc_password ex in
  let resolve = resolve_carry inc in

    print_endline "Original:";
    List.iter ~f:(fun x -> printf "%c " x) ex;
    print_endline "";

    print_endline "Increment:";
    List.iter ~f:(fun x -> print_inc_res x) inc;
    print_endline "";

    print_endline "Resolved:";
    List.iter ~f:(fun x -> printf "%c " x) resolve;
    print_endline "";

   let b = valid_password ex in
    print_endline "Valid:";
    printf "%d\n" (Bool.to_int b);
(*  List.iter ~f:(fun x -> printf "%d\n" (Bool.to_int x)) b;
  print_endline "";*)

(*
  printf "Part 1 answer: %d\n" p1_ans;
  printf "Part 2 answer: %d\n" p2_ans;
*)
