#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

let p1 line = 
  let action paren = 
    match paren with
      | '('->  1
      | ')'-> -1
      | _ -> assert false in
  let total = String.to_list line 
                |> List.map ~f:action 
                |> List.fold_left ~f:(+) ~init:0 in
  printf "Position: %d \n" total

let () =
  let file_to_read = "../input.txt" in
  let lines = In_channel.read_lines file_to_read in
    List.iter ~f: p1 lines;
