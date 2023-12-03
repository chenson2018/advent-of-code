#require "core.top"

open Core
open Core.String
open Core.Int

type number = {
  value: int;
  y: int;
  xmin: int;
  xmax: int; 
}

type symbol = { 
  text: char;
  x: int;
  y: int;
}

type schematic = {
  numbers: number list;
  symbols: symbol list;
}

let fold_schematic s1 s2 =
  {
    numbers = s1.numbers @ s2.numbers;
    symbols = s1.symbols @ s2.symbols
  }

let adj symbol number = 
  number.xmin - 1 <= symbol.x && 
                     symbol.x <= number.xmax + 1 && 
  abs (number.y - symbol.y) <= 1

let parse (input: (char list) list) : schematic =
  let rec aux input num xmax y numbers symbols =
    let xmax = xmax + 1 in
    match input with
    (* the number will continue next call *)
    | ('0'..'9' as i1) :: ('0'..'9' as i2) :: tl -> aux (i2 :: tl) (i1 :: num) xmax y numbers symbols
    (* number terminates *)
    | ('0'..'9' as i1) :: tl -> 
        (
          let value = (i1 :: num) |> List.rev |> of_char_list |> int_of_string in
          let xmin = xmax - (List.length num) in
          let new_num = {value; y; xmin; xmax} in
          aux tl [] (xmax) y (new_num :: numbers) symbols
        )
    (* non-symbol *)
    | '.' :: tl -> aux tl num xmax y numbers symbols
    (* a symbol *)
    | text :: tl -> aux tl num xmax y numbers ({x = xmax; y; text} :: symbols)
    (* exit with accumulated numbers and symbols *)
    | [] -> {numbers; symbols}
  in
  let rows = List.mapi ~f:(fun y -> fun row -> aux row [] 0 y [] []) input in
  List.fold_left ~f:(fold_schematic) ~init:({numbers = []; symbols = []}) rows

let p1_calc schematic : int = 
  schematic.numbers |>
    List.filter ~f:(fun num -> schematic.symbols |> List.exists ~f:(fun sym -> adj sym num)) |>
    List.map ~f:(fun num -> num.value) |>
    List.fold_left ~f:(+) ~init:0

let p2_calc schematic : int =
  schematic.symbols |>
    List.filter_map 
    ~f:
      (
      fun sym -> 
      let adj_nums = schematic.numbers |> List.filter ~f:(adj sym) in
      match sym.text, adj_nums with
        | '*', { value = val1; _} :: {value = val2; _} :: []  -> Some (val1 * val2)
        | _ -> None
      ) |>
    List.fold_left ~f:(+) ~init:0

let () =
  let schematic = In_channel.read_lines "../input.txt" |> List.map ~f:String.to_list |> parse in
  let p1_ans = p1_calc schematic in
  let p2_ans = p2_calc schematic in
    printf "Part 1 answer: %d\n" p1_ans;
    printf "Part 2 answer: %d\n" p2_ans;
