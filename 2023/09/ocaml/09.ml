#require "core.top"
#load "str.cma"
#require "ppx_deriving.show"

open Core
open Core.String
open Core.Int

let rec parse_nums text partial res: int list = 
  match text with
  | ' ' :: tl -> parse_nums tl partial res
  | ('0'..'9' | '-' as i1) :: ('0'..'9' as i2) :: tl -> parse_nums (i2 :: tl) (i1 :: partial) res
  | ('0'..'9' as i1) :: tl -> (
    let value = (i1 :: partial) |> List.rev |> of_char_list |> int_of_string in
    parse_nums tl [] (value :: res)
  )
  | _ -> res

(* these are done on the reversed list to make matching easier *)

let rec diff xs = 
  match xs with
  | one :: two :: [] -> [one - two]
  | one :: two :: tl -> one - two :: diff (two :: tl)
  | _ -> assert false

let extrapolate nums = 
  let rec repeat_diff nums hds = 
    if List.for_all ~f:((=) 0) nums 
    then hds
    else repeat_diff (diff nums) (List.hd_exn nums :: hds)
  in
  let rec extrapolate_iter xs inc = 
    match xs with 
    | [] -> inc
    | hd :: tl -> extrapolate_iter tl (inc + hd)
  in
  extrapolate_iter (repeat_diff nums []) 0

let () = 
  let input = In_channel.read_lines "../input.txt" in
  let nums = input |> List.map ~f:String.to_list |> List.map ~f:(fun xs -> parse_nums xs [] []) in
  let extrapolate_and_sum = fun nums -> nums |> List.map ~f:extrapolate |> List.fold_left ~f:(+) ~init:0 in
  let p1_ans = nums |> extrapolate_and_sum in
    printf "Part 1 answer: %d\n" p1_ans; 
  let p2_ans = nums |> List.map ~f:List.rev |> extrapolate_and_sum in
    printf "Part 2 answer: %d\n" p2_ans;
