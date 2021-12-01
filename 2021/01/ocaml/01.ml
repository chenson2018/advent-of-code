#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

(* general function for successive rolling differences *)

let rec rolling (xs: 'a list) (n: int) =
  match xs with  
  | one :: tl -> (match List.nth tl (n-2) with
                  | Some x -> (x - one) :: (rolling tl n)
                  | None   -> []) 
  | [] -> []
  
let () =
  let input  = In_channel.read_lines "../input.txt" |> List.map ~f:int_of_string in
  let p1_ans = List.filter ~f:(fun x -> x > 0) (rolling input 2) |> List.length in
  let p2_ans = List.filter ~f:(fun x -> x > 0) (rolling input 4) |> List.length in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
