#require "core.top"
#load "str.cma"
#require "ppx_deriving.show"

open Core
open Str
open Core.String
open Core.Int

module Almanac = struct
  type mapping = {
    destination: int;
    source: int;
    range: int;
  } [@@deriving show]
  
  let make_mapping_fn mapping : (int -> int option) = 
  fun value -> 
    if (mapping.source <= value && 
                          value <= mapping.source + mapping.range - 1) 
    then Some (mapping.destination + (value - mapping.source)) 
    else None

  let rec compose_mapping (funs: (int -> int option) list) (value: int) : int = 
    match funs with
    | f :: tl -> (
      match f value with
      | Some res -> res
      | None -> compose_mapping tl value
    )
    | [] -> value

  let min_list_exn l =
    match l with
    | [] -> assert false
    | x::xs -> List.fold_left ~f:min ~init:x xs

  let p1_calc maps value: int =
    maps |> 
      List.map ~f:(List.map ~f:make_mapping_fn) |> 
      List.map ~f:compose_mapping |>
      List.fold_left ~init:value ~f:(fun acc -> fun f -> f acc)

  let location_to_seed maps value: int = 
    let rev_maps = 
      maps |> 
      List.rev |>
      List.map ~f:(List.map  ~f:(fun x -> {x with source = x.destination; destination = x.source})) in
    p1_calc rev_maps value

end  

open Almanac

(* annoyingly long parsing function *)
let parse text : (int list) * (mapping list list) = 
  let rec parse_nums text partial res: int list = 
    match text with
    | ' ' :: tl -> parse_nums tl partial res
    | ('0'..'9' as i1) :: ('0'..'9' as i2) :: tl -> parse_nums (i2 :: tl) (i1 :: partial) res
    | ('0'..'9' as i1) :: tl -> (
      let value = (i1 :: partial) |> List.rev |> of_char_list |> int_of_string in
      parse_nums tl [] (value :: res)
    )
    | _ -> res
  in
  let parse_seeds text: int list = 
    match (String.to_list text) with
    | 's'::'e'::'e'::'d'::'s'::':'::tl -> parse_nums tl [] []
    | _ -> assert false
  in
  let parse_maps text: mapping list =
    let (_,map_raw) = List.split_n (text |> String.split ~on:'\n') 1 in
    map_raw |> List.filter_map ~f:(
      fun t -> 
        match (parse_nums (String.to_list t) [] []) with
        | range :: source :: destination :: _ -> Some {destination; source; range}
        | _ -> None
    )
  in
  let splits = Str.split (Str.regexp "\n\n") text in
  let (seeds_raw,maps_raw) = match splits with | hd :: tl -> (hd,tl) | _ -> assert false in
  let seeds = parse_seeds seeds_raw in
  let maps = List.map ~f:parse_maps maps_raw in
  (seeds,maps)  

module Range = struct
  type range = {
    start: int;
    length: int;
  } [@@deriving show]

  let rec ranges_from_list seeds : range list =
    match seeds with
    | length :: start :: tl -> {length; start} :: ranges_from_list tl
    | _ -> []

  let range_contains range value = 
    range.start <= value && value <= range.start + range.length - 1
end

open Range

let p2_calc maps seeds = 
  let f = location_to_seed maps in
  let rec aux loc = 
    let seed = f loc in
    if List.exists ~f:(fun r -> range_contains r seed) seeds then loc else aux (loc + 1)
  in
  (* this is a guess based on the scale of the data *)
  (* in principle you could binary search for it... *)
  aux 47_500_000

let () = 
  let input = In_channel.read_all "../input.txt" in
  let (seeds,maps) = parse input in
  let p1_ans = List.map ~f:(p1_calc maps) seeds |> min_list_exn in
    printf "Part 1 answer: %d\n" p1_ans;
  let seed_ranges = ranges_from_list seeds in
  let p2_ans = p2_calc maps seed_ranges in
    printf "Part 2 answer: %d\n" p2_ans;
