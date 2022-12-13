#use "topfind"
#thread
#require "core.top"
#require "yojson"

open Core
open Yojson
open Yojson.Basic.Util

type cmp = 
  | Less
  | Eq
  | Greater

let rec container_compare (left: Basic.t) (right: Basic.t) : cmp = 
  match (left,right) with 
  | (`Int x,`Int y) -> if x < y then Less else if x > y then Greater else Eq
  | (`List(lhd :: ltl),`List(rhd::rtl)) -> ( 
    match (container_compare lhd rhd) with
      | Eq -> container_compare (`List(ltl)) (`List(rtl))
      | x  -> x
  )
  | (`Int(x),`List(ys)) -> container_compare (`List([`Int(x)])) right
  | (`List(xs),`Int(y)) -> container_compare left (`List([`Int(y)]))
  | (`List (_::_), `List []) -> Greater
  | (`List [], `List (_::_)) -> Less
  | (`List [], `List [])     -> Eq
  | _ -> assert false

let rec to_pairs (xs: Basic.t list) : (Basic.t * Basic.t) list = 
  match xs with 
    | one :: two :: tl -> (one,two) :: (to_pairs tl)
    | [] -> []
    | one :: tl -> []

let () =
  let json = Yojson.Basic.from_file "../input.json" in
  let input = (json |> member "input" |> to_list ) in
  let pairs = to_pairs input in

  let p1_idx = List.mapi 
                ~f:(fun i -> fun pair -> 
                      match container_compare (fst pair) (snd pair) with 
                      | Less -> (i+1) 
                      | _ -> 0 ) pairs 
  in

  let div1 = `List([`List([`Int(2)])]) in
  let div2 = `List([`List([`Int(6)])]) in

  let sorted = List.sort 
                ~compare:(fun l -> fun r -> 
                            match container_compare l r with 
                            | Eq -> 0 
                            | Less -> -1 
                            | Greater -> 1) 
                (div1 :: div2 :: input)
  in

  let p2_idx = List.mapi 
                ~f:(fun i -> fun t ->
                      match t with 
                        | `List([`List([`Int(2)])]) 
                        | `List([`List([`Int(6)])]) -> i+1 
                        | _                    -> 1 
                   )
               sorted 
  in

  printf "Part 1 answer: %d\n" (List.fold_left ~f:( + ) ~init:0 p1_idx);
  printf "Part 2 answer: %d\n" (List.fold_left ~f:( * ) ~init:1 p2_idx);
