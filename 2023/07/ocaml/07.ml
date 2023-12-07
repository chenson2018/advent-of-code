#require "core.top"
#require "ppx_deriving.show"
#require "ppx_deriving.ord"
#require "ppx_sexp_conv"

open Core
open Core.String
open Core.Int
open Core.Map

module Card = struct
  type card =
    | A
    | K
    | Q
    | J
    | T
    | Num of int
  [@@deriving show, sexp_of]     

  let int_of_card card = 
    match card with
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> 11
    | T -> 10
    | Num x -> x

  let compare c1 c2 = Int.compare (int_of_card c1) (int_of_card c2)

  let rec tiebreaker (c1: card list) (c2: card list) : int = 
    match c1, c2 with
    | hd1 :: tl1, hd2 :: tl2 -> (
      let check = compare hd1 hd2 in
      if check = 0 then (tiebreaker tl1 tl2) else check
    )
    | _ -> 0

  type t = card [@@deriving sexp_of]     
end

module Score = struct
  type score = 
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfKind
    | FullHouse
    | FourOfKind
    | FiveOfKind
  [@@deriving show, ord]
end

module CardFreq = struct
  open Score
  include Card
  include Comparable.Make_plain(Card)

  (* create a map of frequencies *)
  let rec frequency (cards: card list) map = 
    match cards with
    | [] -> map
    | hd :: tl -> (
      let update = fun maybe_count -> 
        match maybe_count with
        | Some c -> Some (c + 1)
        | None -> Some 1
      in
      let map = Map.change map hd ~f:update in
      frequency tl map
    )

  (* each scoring corresponds to a possible mapping *)
  let score_of_cards (cards: card list) : score =
    let empty = Map.of_alist_exn [] in
    let frequencies = frequency cards empty |> Map.data |> List.sort ~compare:Int.compare in
    match frequencies with
    | 1 :: 1 :: 1 :: 1 :: 1 :: [] -> HighCard
    | 1 :: 1 :: 1 ::      2 :: [] -> OnePair
    | 1 :: 2 :: 2 :: [] -> TwoPair
    | 1 :: 1 :: 3 :: [] -> ThreeOfKind
    | 2 :: 3 :: [] -> FullHouse
    | 1 :: 4 :: [] -> FourOfKind
    | 5 :: [] -> FiveOfKind
    | _ -> assert false
end


module OCamlCard = struct
  open Card
  open Score

  type hand = {
    cards: card list;
    bid: int
  }

  let parse_hand line : hand = 
    let parse_card c : card = 
      match Char.get_digit c with
      | Some v -> Num v
      | None -> (
        match c with
        | 'A' -> A
        | 'K' -> K
        | 'Q' -> Q
        | 'J' -> J
        | 'T' -> T
        | _ -> assert false
      )
    in
    match String.to_list line with
    | a::b::c::d::e::' '::bid -> (
      let cards = List.map ~f:parse_card [a; b; c; d; e] in
      let bid = bid |> of_char_list |> int_of_string in
      {cards; bid}
    )
    | _ -> assert false

  let compare (h1: hand) (h2: hand) : int = 
    let s1 = CardFreq.score_of_cards h1.cards in
    let s2 = CardFreq.score_of_cards h2.cards in
    let score_cmp: int = Score.compare_score s1 s2 in
    if score_cmp = 0 then tiebreaker h1.cards h2.cards else score_cmp
end

open OCamlCard
open Card

let p1_calc hands = 
  hands |> 
    List.sort ~compare:(OCamlCard.compare) |>
    List.mapi ~f:(fun i -> fun h -> (i + 1) * h.bid) |> 
    List.fold_left ~f:(+) ~init:0

let () = 
  let input = In_channel.read_lines "../input.txt" in
  let hands = input |> List.map ~f:parse_hand in
  let p1_ans = p1_calc hands in
    printf "Part 1 answer: %d\n" p1_ans
