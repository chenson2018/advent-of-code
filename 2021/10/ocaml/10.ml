#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core
open Hashtbl

type paired_char = 
  {
  c: char;
  score: int;
  }

let error_score_hash       = Hashtbl.of_alist_exn (module Char)
                             [
                               (')', {c = '('; score = 3    });
                               (']', {c = '['; score = 57   });
                               ('}', {c = '{'; score = 1197 });
                               ('>', {c = '<'; score = 25137})
                             ];;

let incomplete_score_hash  = Hashtbl.of_alist_exn (module Char)
                             [
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
  let rec aux (left: char list) (right: char list) : (char list * char list) = 
    match left, right with
    | current :: tl_l, next :: tl_r -> (match Hashtbl.find error_score_hash next with
                                        | None -> aux (next::left) tl_r                        (* if not a closing bracket, push to left and continue     *)
                                        | Some req_open -> if (Char.equal current req_open.c)  (* otherwise, check that it is the correct closing bracket *)
                                                           then aux tl_l tl_r
                                                           else (left, right))
    | _ -> (left, right)
  in
  (* here we make the initial character to the left list *)
  match xs with
  | hd :: tl -> aux [hd] tl
  | _        -> assert false

let error_score (xs: char list): int = 
  match xs with
  | []     -> 0
  | err::_ -> (match Hashtbl.find error_score_hash err with
               | Some x -> x.score
               | None   -> assert false)

let incomplete_score (xs: char list): int = 
  let advance (cum: int) (c: char) : int = 
    match Hashtbl.find incomplete_score_hash c with
    | None   -> assert false
    | Some x -> 5*cum + x.score
  in
  List.fold_left ~f:advance ~init:0 xs

let () =
  let input  = In_channel.read_lines "../input.txt" |> List.map ~f:String.to_list in
  
  let end_state = input |> List.map ~f:parse_line in
  let error_lists = List.map ~f:snd end_state in
  let p1_ans = error_lists |> 
                 List.map ~f:error_score |>  
                 List.fold_left ~f:(+) ~init:0 in

     printf "Part 1 answer: %d\n" p1_ans;


  let incomplete = end_state |>
                   List.filter ~f:(fun state -> match state with 
                                                | (_,[]) -> true              (* if there are no errors, second tuple entry will be empty   *) 
                                                | _      -> false)            (* could also just use the error_score function               *)
  in 
  let remaining_symbols  = List.map ~f:fst incomplete in                      (* grab the first tuple element, which are left to be matched *)
  let p2_ans = List.nth_exn 
               ( remaining_symbols |> 
                   List.map ~f:incomplete_score |> 
                   List.sort ~compare:Int.compare
               ) 
               (* median index for an odd length list *)
               (List.length remaining_symbols / 2) 
  in 

    printf "Part 2 answer: %d\n" p2_ans;

