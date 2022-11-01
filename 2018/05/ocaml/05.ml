#use "topfind"
#thread
#require "core.top"
#require "core.syntax"

open Core

let polar (c: char) : char =
  match Char.is_lowercase c with 
  | true  -> Char.uppercase c 
  | false -> Char.lowercase c 

let polar_pair (c1 : char) (c2 : char) : bool = 
  Char.equal c1 (polar c2)

let react (xs: char list) : char list = 
  let rec aux bck fwd =
    match (bck, fwd) with
    | (b0 :: b_tl, f0 :: f_tl) when (polar_pair b0 f0) -> aux b_tl f_tl
    | (_,f0 :: tl) -> aux (f0 :: bck) tl
    | (_,[]) -> List.rev bck
  in
  aux [] xs

let remove_pair (xs : char list) (c : char) : char list = 
  List.filter ~f:(fun x -> not (Char.equal x        c)   && 
                           not (Char.equal x (polar c))) 
  xs

let () = 
  let init  = In_channel.read_all "../input.txt" |> 
                String.to_list |> 
                List.filter ~f:(fun x -> not (Char.equal x '\n'))
  in

  let p1_ans  = react init |> List.length in

  let letters = "abcdefghijklmnopqrstuvwxyz" |> String.to_list in

  let p2_lens = List.map ~f:(fun c -> remove_pair init c |> react |> List.length ) letters in

  let p2_ans = match List.min_elt ~compare:Int.compare p2_lens with
               | Some x -> x
               | _  -> assert false
  in

    printf "Part 1 answer: %d\n" p1_ans; 
    printf "Part 2 answer: %d\n" p2_ans; 
