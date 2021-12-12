#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core
open Hashtbl

type closing = 
  {
  c: char;
  score: int;
  }

let scores_hash  = Hashtbl.of_alist_exn (module Char) [
                                                        (')', {c = '('; score = 3    });
                                                        (']', {c = '['; score = 57   });
                                                        ('}', {c = '{'; score = 1197 });
                                                        ('>', {c = '<'; score = 25137})
                                                      ];;

(* 

I successively take each character and move it to another list, eliminating matching paris as needed
and exiting when a match is invalid

Small example: "[{<()}"

   [, {<()}    initial state
  {[,  <()}    each time we have an opening, move it to the left list
 <{[,   ()}
(<{[,    )}    when we have a match, remove them
 <{[,     }    here we have a mismatch, so exit

*)

let score_corrupt (xs: char list) : int = 
  let rec aux left right = 
    match left, right with
    | current :: tl_l, next :: tl_r -> (match Hashtbl.find scores_hash next with
                                        | None -> aux (next::left) tl_r
                                        | Some req_open -> if (Char.equal current req_open.c) 
                                                           then aux tl_l tl_r
                                                           else req_open.score)
    | _ -> 0
  in
  match xs with
  | hd :: tl -> aux [hd] tl
  | _        -> assert false

let () =
  let input  = In_channel.read_lines "../input.txt" |> List.map ~f:String.to_list in
  let p1_ans = input |> List.map ~f:score_corrupt |> List.fold_left ~f:(+) ~init:0 in
    printf "Part 1 answer: %d\n" p1_ans;
