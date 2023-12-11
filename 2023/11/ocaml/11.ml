#require "core.top"
#require "ppx_deriving.show"

open Core
open Core.Int
open Core.Map

type galaxy = {
  x: int;
  y: int;
} [@@deriving show]

let parse lines : galaxy list = 
  let lines = List.map ~f:(String.to_list) lines in
  let rec parse_line line x y = 
    match line with
    | '#' :: tl -> {x; y} :: parse_line tl (x + 1) y
    | _   :: tl -> parse_line tl (x + 1) y
    | [] -> []
  in
  let lines = List.mapi ~f:(fun y -> fun line -> parse_line line 0 y) lines in
  List.fold_left ~f:(@) ~init:[] lines

(* nice way to combine galaxies *)
let fold_galaxy f g1 g2 =
  {
    x = f g1.x g2.x;
    y = f g1.y g2.y;
  }

(* steps (Manhattan distance) between two galaxies *)
let steps (g1,g2) = 
  abs (g1.x - g2.x) + abs (g1.y - g2.y)

(* get all combinations of size 2 *)
let rec choose_2 xs =
  match xs with
  | [] -> []
  | hd :: tl -> (List.map ~f:(fun x -> (x, hd)) tl) @ choose_2 tl

(* cumulative sum - reusing this from 2015 AoC! *)
let cumulative (l: int list) : int list = 
  let rec aux (l: int list) (a: int) : int list = 
    match l with
    | [] -> []
    | x::xs -> (a+x) :: aux xs (a+x) in
  aux l 0

let expand factor galaxies = 
  let factor = if factor = 1 then 1 else factor - 1 in
  let max_galaxy = List.fold_left ~f:(fold_galaxy Int.max) ~init:{x = 0; y = 0} galaxies in
  (* a map of how far to shift all galaxy y values *)
  let y_map = 
    List.range ~start:`inclusive ~stop:`inclusive 0 max_galaxy.y |>
    List.map ~f:(fun y -> if (galaxies |> List.exists ~f:(fun g -> g.y = y)) then 0 else factor) |>
    cumulative |>
    List.mapi ~f:(fun y -> fun shift -> (y, shift)) |>
    Core.Map.of_alist_exn (module Int)
  in
  (* a map of how far to shift all galaxy x values *)
  let x_map = 
    List.range ~start:`inclusive ~stop:`inclusive 0 max_galaxy.x |>
    List.map ~f:(fun x -> if (galaxies |> List.exists ~f:(fun g -> g.x = x)) then 0 else factor) |>
    cumulative |>
    List.mapi ~f:(fun x -> fun shift -> (x, shift)) |>
    Core.Map.of_alist_exn (module Int)
  in
  galaxies |> 
    List.map ~f:(fun g -> {x = g.x + find_exn x_map g.x ; 
                           y = g.y + find_exn y_map g.y ;})

let sum_steps galaxies = 
  galaxies |>
    choose_2 |>
    List.map ~f:steps |>
    List.fold_left ~f:(+) ~init:0

let () = 
  let galaxies = In_channel.read_lines "../input.txt" |> parse in
  let p1_ans = galaxies |> expand 1       |> sum_steps in
  let p2_ans = galaxies |> expand 1000000 |> sum_steps in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
