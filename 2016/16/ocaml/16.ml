#use "topfind"
#thread
#require "core.top"

open Core

let flip (c : char) : char = 
  match c with
  | '1' -> '0'
  | '0' -> '1'
  | _   -> assert false

let rec p1_iter (xs : char list) (len : int) : char list = 
  if (List.length xs) >= len 
  then 
    (List.take xs len) 
  else 
    let next  = List.fold_left 
                  ~f:List.append 
                  ~init:[] 
                  [ 
                      xs 
                    ; ['0']
                    ; xs |> 
                        List.rev |> 
                        List.map ~f:flip
                  ] 
    in
    p1_iter next len

let rec checksum (xs : char list) : char list = 
 let rec checksum_iter ys = 
  match ys with
  | '1' :: '1' :: tl -> '1' :: (checksum_iter tl)
  | '0' :: '0' :: tl -> '1' :: (checksum_iter tl)
  | _   :: _   :: tl -> '0' :: (checksum_iter tl)
  | [] -> []
  | _ -> assert false
 in

 if (List.length xs) mod 2 = 0
 then
  checksum (checksum_iter xs)
 else
  xs

let () = 
  let init  = "10001110011110000" |> String.to_list in
  let p1_ans=  checksum (p1_iter init 272) in
    printf "Part 1 answer: ";  
    List.iter ~f:(printf "%c") p1_ans;
    printf "\n";  
