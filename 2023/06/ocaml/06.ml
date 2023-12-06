#require "core.top"

open Core
open Core.Float
open Core.Int

type race = {
  distance: float;
  time: float;
}

let wins race = 
  (* this is the quadratic formula *)
  (* I put an upper tolerance on the distance to represent that we want to beat the record *)
  (* by rounding up/down from these roots, we get the positive parts of the quadratic *)
  let tolerance = 0.0001 in
  let discriminant = sqrt (square race.time -. 4. *. (race.distance +. tolerance)) in
  let lower = (race.time -. discriminant) /. 2. |> Float.round_up   |> Float.to_int in
  let upper = (race.time +. discriminant) /. 2. |> Float.round_down |> Float.to_int in
  upper - lower + 1

let () = 
  let races = [
    {distance = 298. ; time = 49.};
    {distance = 1185.; time = 78.};
    {distance = 1066.; time = 79.};
    {distance = 1181.; time = 80.};
  ]
  in
let p1_ans = races |> List.map ~f:wins |> List.fold_left ~f:( * ) ~init:1 in
    printf "Part 1 answer: %d\n" p1_ans;
let p2_ans = wins {time = 49787980.; distance = 298118510661181.} in
    printf "Part 2 answer: %d\n" p2_ans;

