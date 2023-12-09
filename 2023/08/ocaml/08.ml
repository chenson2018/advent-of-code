#require "core.top"
#load "str.cma"
#require "ppx_deriving.show"

open Core
open Str
open Core.Map
open Core.String

type instruction = 
  | Left
  | Right

let instruction_of_char c = 
  match c with
  | 'L' -> Left
  | 'R' -> Right
  | _ -> assert false

type network = {
  instructions: instruction list;
  map: (string, string * string, comparator_witness) Poly.map
}

let parse text = 
  let (instructions_raw, nodes_raw) = 
    match Str.split (Str.regexp "\n\n") text with
    | ins :: nodes :: [] -> (ins,nodes)
    | _ -> assert false
  in
  let instructions = instructions_raw |> String.to_list |> List.map ~f:instruction_of_char in
  let map = 
    nodes_raw |>
      String.split ~on:'\n' |>
      List.filter ~f:(fun line -> line <> "") |>
      List.map ~f:(
        fun s ->
        let pattern = regexp "\\([A-Z]+\\) = (\\([A-Z]+\\), \\([A-Z]+\\))" in
        let _ = Str.search_forward pattern s 0 in
        let name  = Str.matched_group 1 s in
        let left  = Str.matched_group 2 s in
        let right = Str.matched_group 3 s in
        (name, (left,right))
      ) |>
      Core.Map.of_alist_exn (module String)
  in
  {instructions; map}


let traverse network start (stop: string -> bool) = 
  let iter instruction name = 
    let (left,right) = Core.Map.find_exn network.map name in
      match instruction with
      | Left -> left
      | Right -> right
  in
  let rec aux instructions name count = 
    if stop name
      then count 
      else 
        match instructions with
        | [] -> aux network.instructions name count
        | ins :: tl -> (
          let next_name = iter ins name in
          aux tl next_name (count + 1)
        )
  in
  aux network.instructions start 0

(* from https://rosettacode.org/wiki/Least_common_multiple#OCaml *)
module LCM = struct
  open Core.Int

  let rec gcd u v =
    if v <> 0 then (gcd v (u mod v))
    else (abs u)
  
  let lcm m n =
    match m, n with
    | 0, _ | _, 0 -> 0
    | m, n -> abs (m * n) / (gcd m n)
end

let p2_calc network = 
  network.map |>
    Core.Map.keys |>                                                                         (* get node names *)
    List.filter ~f:(fun n -> Str.last_chars n 1 = "A") |>                                    (* filter for names ending in 'A' *)
    List.map ~f:(fun start -> traverse network start (fun n -> Str.last_chars n 1 = "Z")) |> (* map each path to a 'Z' ending *)
    List.fold_left ~f:LCM.lcm ~init:1                                                        (* fold LCM to find when they all land on a 'Z' ending at the same time *)

let () = 
  let input = In_channel.read_all "../input.txt" in
  let network = parse input in
  let p1_ans = traverse network "AAA" ((=) "ZZZ") in
    printf "Part 1 answer: %d\n" p1_ans;
  let p2_ans = p2_calc network in
    printf "Part 2 answer: %d\n" p2_ans;
