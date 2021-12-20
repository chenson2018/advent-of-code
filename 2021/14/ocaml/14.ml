#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str
open Hashtbl

(* for the sake of my sanity I split up the input so that input.txt only has the hash values *)

let parse_rule (s: string) : string * string = 
  let parse (n: int) (g: int): string =
    Str.matched_group g s
  in

  let pattern = regexp "\\([A-Z]+\\) -> \\([A-Z]+\\)" in
  let n = Str.search_forward pattern s 0 in

  (parse n 1, parse n 2)

let rec advance (l: char list) (ht: (string, string) t) : char list = 
  match l with
  | one :: two :: tl -> (match Hashtbl.find ht ((String.make 1 one)^(String.make 1 two)) with
                         | Some x -> one :: (String.get x 0) :: (advance (two::tl) ht)
                         | None   -> assert false
                        )
  | [last] -> [last]
  | []     -> []

(* found on SO but I can't find the link again *)
(* allows a fold to reapeat i times            *)
let rec foldi i f acc =
    if i <= 0 then acc else foldi (pred i) f (f acc)


let () = 
  let input = In_channel.read_lines "../input.txt" in
  let rules = Hashtbl.of_alist_exn (module String) (List.map ~f:parse_rule input) in
  let init  = String.to_list "BSONBHNSSCFPSFOPHKPK" in
  let ten_iter = foldi 10 (fun x -> advance x rules) init in

  (* cheating a bit since I know each element occurs at least twice *)
  let letters = List.find_all_dups ~compare:Char.compare ten_iter in

  (* counts of each letter *)
  let counts = List.map 
                 ~f:(
                     fun x -> List.count 
                                ~f:(fun y -> Char.equal x y) 
                              ten_iter
                    )
               letters in

  let max = List.max_elt ~compare:Int.compare counts in
  let min = List.min_elt ~compare:Int.compare counts in

  let p1_ans = match max, min with
               | Some x, Some y -> x - y
               | _              -> assert false
  in

    printf "Part 1 answer: %d\n" p1_ans;

