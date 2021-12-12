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

let scores_hash2  = Hashtbl.of_alist_exn (module Char) [
                                                        ('(', {c = ')'; score = 1});
                                                        ('[', {c = ']'; score = 2});
                                                        ('{', {c = '}'; score = 3});
                                                        ('<', {c = '>'; score = 4})
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

let parse_line (xs: char list) : (char list * char list) = 
  let rec aux left right = 
    match left, right with
    | current :: tl_l, next :: tl_r -> (match Hashtbl.find scores_hash next with
                                        | None -> aux (next::left) tl_r
                                        | Some req_open -> if (Char.equal current req_open.c) 
                                                           then aux tl_l tl_r
                                                           else (left, right))
    | _ -> (left, right)
  in
  match xs with
  | hd :: tl -> aux [hd] tl
  | _        -> assert false

let score (xs: char list): int = 
  match xs with
  | []     -> 0
  | err::_ -> (match Hashtbl.find scores_hash err with
               | Some x -> x.score
               | None   -> assert false)

let score2 xs = 
  let advance cur c = 
    match Hashtbl.find scores_hash2 c with
    | None -> assert false
    | Some x -> 5*cur + x.score
  in
  List.fold_left ~f:advance ~init:0 xs

let () =
  let input  = In_channel.read_lines "../input.txt" |> List.map ~f:String.to_list in
  
  let end_state = input |> List.map ~f:parse_line in


  let p1_ans = end_state |> 
                 List.map ~f:(fun state -> score (snd state)) |>  
                 List.fold_left ~f:(+) ~init:0 in

     printf "Part 1 answer: %d\n" p1_ans;


  let incomplete = List.filter ~f:(fun state -> (score (snd state)) = 0) end_state in

  let p2_score = incomplete |> List.map ~f:(fun x -> score2 (fst x)) in

  let p2_ans = List.nth_exn ( p2_score |> List.sort ~compare:Int.compare) (List.length p2_score / 2) in

    printf "Part 2 answer: %d\n" p2_ans;

(*  let p1_ans = input |> List.map ~f:score_corrupt |> List.fold_left ~f:(+) ~init:0 in
    printf "Part 1 answer: %d\n" p1_ans;
*)
