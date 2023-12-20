#require "core.top"
#load "str.cma"
#require "ppx_deriving.show"
#require "ppx_deriving.eq"

open Core
open Core.Int
open Core.Map
open Str
open Core.String 

type rating = {
  x: int;
  m: int;
  a: int;
  s: int;
}

let rating_num r = r.x + r.m + r.a + r.s

type category = 
  | X
  | M
  | A
  | S
  [@@deriving show]

let category_of_char c = 
  match c with 
  | 'x' -> X
  | 'm' -> M
  | 'a' -> A
  | 's' -> S
  | _ -> assert false

let access rating category = 
  match category with 
  | X -> rating.x
  | M -> rating.m
  | A -> rating.a
  | S -> rating.s

type terminal = 
  | Accept
  | Reject
  [@@deriving show, eq]

type result = 
  | Name of string
  | Terminal of terminal
  [@@deriving show]

let result_of_string s = 
  match s with
  | "A" -> Terminal Accept
  | "R" -> Terminal Reject
  | _   -> Name s

type rule = {
  category: category;
  op: int -> int -> bool;
  value: int;
  name: result;
} 

type workflow = {
  name: string;
  rules: rule list;
  default: result;
}

(* apply a single rule, returning the result or None *)

let apply_rule rating rule = 
  let comp = access rating rule.category in
  if (rule.op comp rule.value) then Some rule.name else None

(* given a workflow and a rating, apply rules to the rating until a match is found, or return the default *)

let interpret_workflow workflow rating = 
  let rec aux rating rules = 
    match rules with
    | rule :: tl -> (
        match apply_rule rating rule with
        | Some res -> res
        | None -> aux rating tl
    )
    | [] -> workflow.default
  in
  aux rating workflow.rules

(* tedious parsing... *)

let parse text = 
  let rec parse_nums partial xs = 
    match xs with
    | ('0'..'9' as i1) :: ('0'..'9' as i2) :: tl -> parse_nums (i1 :: partial) (i2 :: tl)
    | ('0'..'9' as i1) :: tl ->  (
      let value = (i1 :: partial) |> List.rev |> of_char_list |> int_of_string in
      (value, tl)
    )
    | _ -> assert false
  in
  let (workflows_raw, ratings_raw) = 
    match Str.split (Str.regexp "\n\n") text with
    | ins :: nodes :: [] -> (String.split ~on:'\n' ins, String.split ~on:'\n' nodes)
    | _ -> assert false
  in
  let parse_wf text = 
    let (name,body) = 
      match String.split_on_chars ~on:['{'; '}'] text with
      | name :: body :: _ :: [] -> (name,body)
      | _ -> assert false
    in
    let rules = String.split ~on:',' body in
    let (rules, fin) = List.split_n rules (List.length rules - 1) in
    let parse_rule text = 
      let (category,op,remain) = 
        match String.to_list text with
        | c :: '>' :: tl -> (category_of_char c, Int.(>), tl)
        | c :: '<' :: tl -> (category_of_char c, Int.(<), tl)
        | _ -> assert false
      in
      let (value, remain) = parse_nums [] remain in
      let name = match remain with
        | ':' :: tl -> result_of_string (String.of_char_list tl)
        | _ -> assert false
      in
      {category; op; value; name}
    in
    {
     name; 
     rules = rules |> List.map ~f:parse_rule; 
     default = result_of_string (List.hd_exn fin)
    }
  in
  let parse_rating text =
    match String.split_on_chars ~on:['{'; '}'; 'x'; 'm'; 'a'; 's'; ','; '='] text with
    | _ :: _ :: _ :: x :: _ :: _ :: m :: _ :: _ :: a :: _ :: _ :: s :: _ -> (
      let x = int_of_string x in
      let m = int_of_string m in
      let a = int_of_string a in
      let s = int_of_string s in
      Some {x; m; a; s}
    )
    | _ -> None
  in
  (workflows_raw |> List.map ~f:parse_wf, ratings_raw |> List.filter_map ~f:parse_rating)

let rec p1_calc workflows rating name = 
  (* start by finding the current workflow *)
  let wf = workflows |> 
    List.filter ~f:(fun wf -> wf.name = name) |> 
    List.hd_exn 
  in
  (* check if the workflow leads to another or a terminal result *)
  match interpret_workflow wf rating with
  | Name new_name -> p1_calc workflows rating new_name
  | Terminal res -> res

let () = 
    let lines = In_channel.read_all "../input.txt" in
    let (workflows, ratings) = parse lines in
    let p1_ans = ratings |> 
      List.filter ~f:(fun r -> equal_terminal (p1_calc workflows r "in") Accept) |> 
      List.map ~f:rating_num |> 
      List.fold_left ~f:(+) ~init:0 
    in
      printf "Part 1 answer: %d\n" p1_ans
