#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core

let p1_valid (xs: char list) : bool = 
  let three_vowels (xs: char list) : bool = 
    let rec aux ys = 
      match ys with
      | [] -> []
      | 'a'::tl | 'e'::tl | 'i'::tl | 'o'::tl | 'u'::tl-> 1 :: (aux tl)
      | _::tl -> 0 :: (aux tl)
    in
    let count = List.fold_left ~f:(+) ~init:0 (aux xs) in
    (count >= 3)
  in
    
  let double_letter (xs: char list) : bool =
    let rec aux ys =
      match ys with
      | [] | _::[] -> false
      | one :: two :: tl when (Char.equal one two) -> true
      | one :: two :: tl                           -> aux (two::tl)
    in
    aux xs
  in

  let no_forbidden (xs: char list) : bool = 
    let rec aux ys =
      match ys with
      | 'a'::'b'::tl -> false
      | 'c'::'d'::tl -> false
      | 'p'::'q'::tl -> false
      | 'x'::'y'::tl -> false
      | one :: two :: tl -> aux (two::tl)
      | [] | _::[] -> true
    in
    aux xs
  in
  ((three_vowels xs) && (double_letter xs) && (no_forbidden xs))

let p2_valid (xs: char list) : bool = 
  let distinct_double (xs: char list) : bool =
    let rec aux ys =
      match ys with
      | a :: b :: c :: tl when ((Char.equal a b) && (Char.equal b c)) -> (a::a::[]) :: (aux (a::tl))
      | a :: b :: tl -> (a::b::[]) :: (aux (b::tl))
      | _::[] | []   -> []
    in
    let twos = aux xs in
    List.contains_dup ~compare:(Poly.compare) twos
  in

  let sandwich (xs: char list) : bool = 
    let rec aux ys =
      match ys with
      | a :: b :: c :: tl when (Char.equal a c) -> true
      | a :: b :: tl                            -> aux (b :: tl)
      | _::[] | []                              -> false
    in
    aux xs
  in
  (distinct_double xs) && (sandwich xs)

let () = 
  let input  = "../input.txt" |>
                 In_channel.read_lines |> 
                 List.map ~f:String.to_list 
  in

  let p1_ans = input |> 
                 List.filter ~f:p1_valid |> 
                 List.length 
  in

  let p2_ans = input |> 
                 List.filter ~f:p2_valid |> 
                 List.length 
  in
  
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
