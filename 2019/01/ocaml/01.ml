#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core

let () = 
  let input  = In_channel.read_lines "../input.txt" |> 
                 List.map ~f:int_of_string 
  in

  let p1_ans = input |> 
                 List.map ~f:(fun x -> (x / 3) - 2) |> 
                 List.fold_left ~f:(+) ~init:0 
  in

  printf "Part 1 answer: %d\n" p1_ans;
