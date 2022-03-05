#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core

let single_fuel (mass: int) : int =
 (mass / 3) - 2 

(* 
logic here is a tiny bit convoluted
the first match case is so that I can exit out of the list of 
length one without counting the original mass
*)
let fuel_for_fuel (mass: int) : int = 
  let rec aux l = 
    match l with
    | x::[] -> aux [single_fuel x; 0]
    | x::xs when ((single_fuel x) < 0) -> l
    | x::xs -> aux ((single_fuel x) :: l)
    | [] -> assert false
  in
  List.fold_left ~f:(+) ~init:0 (aux [mass])

let () = 
  let input  = In_channel.read_lines "../input.txt" |> 
                 List.map ~f:int_of_string 
  in

  let p1_ans = input |> 
                 List.map ~f:single_fuel |> 
                 List.fold_left ~f:(+) ~init:0 
  in


  let p2_ans = (input |> 
                 List.map ~f:fuel_for_fuel |> 
                 List.fold_left ~f:(+) ~init:0)
  in

  printf "Part 1 answer: %d\n" p1_ans;
  printf "Part 2 answer: %d\n" p2_ans;
