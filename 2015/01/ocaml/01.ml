#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

let p1 line =
  let position = ref 0 in        
  for j = 0 to String.length line - 1 do
    match line.[j] with
    | '('-> position := !position + 1
    | ')'-> position := !position - 1
    | _ -> assert false
  done;
  
  printf "Position: %d \n" !position

let () =
  let file_to_read = "../input.txt" in
  let lines = In_channel.read_lines file_to_read in
    List.iter ~f: p1 lines;
