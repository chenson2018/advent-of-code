#require "core.top"

open Core
open Core.String
open Core.Int

type color = {
  red: int;
  green: int;
  blue: int;
}

(* nice way to combine colors *)
let color_binop (f: int -> int -> int) (c1: color) (c2: color) : color =
  {
    red   = f c1.red   c2.red;
    green = f c1.green c2.green;
    blue  = f c1.blue  c2.blue;
  }

let color_cmp (f: int -> int -> bool) (c1: color) (c2: color) : bool =
  f c1.red   c2.red   &&
  f c1.green c2.green &&
  f c1.blue  c2.blue

type game = {
  id: int;
  draws: color list;
}

(* function to init a color *)
let new_color = {red = 0; green = 0; blue = 0}

let parse (line: string): game = 
  (* either keep building or finish and append a draw *)
  let iter (draw: color) (tl: char list) (f: char list -> color -> color list) : color list = 
    match tl with
    | ','::tl' -> f tl' draw
    | ';'::tl' -> draw :: (f tl' new_color)
    | _ -> draw :: (f  [] new_color)
  in

  let int_parse xs = int_of_string (of_char_list xs) in

  let rec aux (input: char list) (draw: color): color list = 
    match input with
    | ' '::n::    ' '::'g'::'r'::'e'::'e'::'n'::tl -> iter {draw with green = int_parse [n    ]} tl aux
    | ' '::n::n1::' '::'g'::'r'::'e'::'e'::'n'::tl -> iter {draw with green = int_parse [n; n1]} tl aux
    | ' '::n::    ' '::'r'::'e'::'d'          ::tl -> iter {draw with red =   int_parse [n    ]} tl aux
    | ' '::n::n1::' '::'r'::'e'::'d'          ::tl -> iter {draw with red =   int_parse [n; n1]} tl aux
    | ' '::n::    ' '::'b'::'l'::'u'::'e'     ::tl -> iter {draw with blue =  int_parse [n    ]} tl aux
    | ' '::n::n1::' '::'b'::'l'::'u'::'e'     ::tl -> iter {draw with blue =  int_parse [n; n1]} tl aux
    | _ -> []
  in

  let (id, tl) = 
    match (String.to_list line) with
      | 'G'::'a'::'m'::'e'::' '::        i1::':'::tl -> (int_parse [i1]        , tl)
      | 'G'::'a'::'m'::'e'::' '::    i1::i2::':'::tl -> (int_parse [i1; i2]    , tl)
      | 'G'::'a'::'m'::'e'::' '::i1::i2::i3::':'::tl -> (int_parse [i1; i2; i3], tl)
      | _ -> assert false
  in
  { id; draws = aux tl new_color }

let p1_calc (g: game) : int =
  if List.for_all ~f:(color_cmp (>=) {red = 12; green = 13; blue = 14}) g.draws
  then g.id
  else 0

let p2_calc (g: game) : int = 
  let draw = List.fold_left ~f:(color_binop max) ~init:(new_color) g.draws in
  draw.red * draw.green * draw.blue

let () =
  let input   = In_channel.read_lines "../input.txt" |> List.map ~f:(parse) in 
  let p1_ans = List.map ~f:(p1_calc) input |> List.fold_left ~f:(+) ~init:0 in
  let p2_ans = List.map ~f:(p2_calc) input |> List.fold_left ~f:(+) ~init:0 in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
