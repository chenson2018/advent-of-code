#use "topfind"
#thread
#require "core.top"
#require "core.syntax"
#load "str.cma"

open Core
open Str

(* 
easy with a loop, but I'm trying to understand streams/lazy eval in Ocaml
https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec24-streams/streams.htm  
is where I found these functions 
*)

(* the natural numbers *)
let rec from n =
  Seq.Cons (n, fun () -> from (n + 1))

(* head of a stream *)
let hd s =
  match s with
  | Seq.Nil -> failwith "hd"
  | Seq.Cons (x, _) -> x

(* tail of a stream *)
let tl s =
  match s with
  | Seq.Nil -> failwith "tl"
  | Seq.Cons (_, g) -> g () (* get the tail by evaluating the thunk *)

(* n-th element of a stream *)
let rec nth s n =
  if n = 0 then hd s else nth (tl s) (n - 1)

let rec filter f s =
  match s with Seq.Nil -> Seq.Nil
  | Seq.Cons (x, g) ->
      if f x then Seq.Cons (x, fun () -> filter f (g ()))
      else filter f (g ())

let rec map f s =
  match s with Seq.Nil -> Seq.Nil
  | _ -> Seq.Cons (f (hd s), fun () -> map f (tl s))

(* finally working on the problem... a function to get a hash *)

let hash_cond key num regex = 
  let get_hash key num = 
    Md5_lib.to_hex (Md5_lib.string (key^(string_of_int num)))
  in
  let pattern = regexp regex in
  string_match pattern ( get_hash key num ) 0

(* 
this works but is a bit slow
I wonder if there is some logic that could decide where to start searching
*)

let () =
  let key       = "iwrupvqb" in
  let regex_p1  = "^00000" in
  let regex_p2  = "^000000" in
  let naturals  = from 0 in
  let p1_stream = filter (fun x -> hash_cond key x regex_p1 ) naturals in
  let p2_stream = filter (fun x -> hash_cond key x regex_p2 ) naturals in
    printf "Part 1 answer: %d\n" (hd p1_stream);
    printf "Part 2 answer: %d\n" (hd p2_stream);

