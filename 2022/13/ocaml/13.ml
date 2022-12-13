#use "topfind"
#thread
#require "core.top"

open Core

type container = 
  | Num of int 
  | Con of container list


type cmp = 
  | Less
  | Eq
  | Greater

let rec container_compare (left : container) (right: container) : cmp = 
  match (left,right) with
    | (Num(x),Num(y)) -> if x < y then Less else if x > y then Greater else Eq
    | (Con(lhd :: ltl),Con(rhd::rtl)) -> ( 
      match (container_compare lhd rhd) with
        | Eq -> container_compare (Con(ltl)) (Con(rtl))
        | x  -> x
    )
    | (Num(x),Con(ys)) -> container_compare (Con([Num(x)])) right
    | (Con(xs),Num(y)) -> container_compare left (Con([Num(y)]))
    | (Con (_::_), Con []) -> Greater
    | (Con [], Con (_::_)) -> Less
    | (Con [], Con [])     -> Eq

let rec to_pairs (xs: container list) : (container * container) list = 
  match xs with 
    | one :: two :: tl -> (one,two) :: (to_pairs tl)
    | [] -> []
    | one :: tl -> []

let () = 
  let input = [

Con([Con([Con([Num(6);Num(10)]);Con([Num(4);Num(3);Con([Num(4)])])])]);
Con([Con([Num(4);Num(3);Con([Con([Num(4);Num(9);Num(9);Num(7)])])])]);
Con([Con([Num(6);Con([Con([Num(3);Num(10)]);Con([]);Con([]);Num(2);Num(10)]);Con([Con([Num(6);Num(8);Num(4);Num(2)])])]);Con([])]);
Con([Con([Num(6);Con([]);Con([Num(2);Con([Num(6);Num(2)]);Num(5)])])]);
Con([Con([Num(4);Con([]);Num(0)]);Con([]);Con([Num(1);Con([Num(2);Con([Num(3)])]);Num(8);Num(5)]);Con([]);Con([Num(10)])]);
Con([Con([Con([Num(9);Con([Num(10);Num(2)])]);Con([Num(5);Con([Num(8);Num(6)]);Num(6);Num(4);Con([Num(9);Num(10)])]);Num(10)]);Con([Num(6);Num(9);Con([Con([Num(10);Num(1)])])]);Con([]);Con([Con([Con([Num(6);Num(6);Num(6);Num(8);Num(8)]);Num(3)]);Con([Num(6);Con([])]);Con([Con([Num(0);Num(5);Num(5);Num(10)]);Num(4);Num(1);Con([Num(3);Num(3)])]);Num(8)])]);
Con([Con([Con([Con([Num(5);Num(3)]);Num(1);Con([Num(10);Num(2);Num(9);Num(7);Num(3)])]);Con([Con([Num(9);Num(1);Num(1)]);Con([Num(10)])]);Con([Con([Num(1);Num(6);Num(3);Num(10)]);Num(7);Con([Num(2)]);Num(4);Num(1)]);Num(0);Num(5)]);Con([Con([Con([Num(10);Num(1);Num(3);Num(7);Num(1)]);Con([Num(8);Num(8);Num(8);Num(0);Num(9)]);Num(1)]);Con([Num(8);Con([Num(1);Num(1);Num(10);Num(7)])]);Con([Con([])])]);Con([])]);
Con([Con([]);Con([Con([Con([Num(7)]);Con([]);Con([Num(9);Num(5)]);Num(5);Con([Num(9);Num(7);Num(8);Num(9)])])]);Con([Num(3);Num(6);Con([Num(7);Con([])])]);Con([Num(6);Num(9)])]);
Con([Con([Con([Con([Num(9);Num(8);Num(6)])]);Con([Con([Num(10);Num(6);Num(8);Num(2);Num(0)])]);Num(8);Num(3);Num(3)]);Con([Con([]);Con([Con([Num(0);Num(4);Num(1)]);Num(1);Con([]);Num(4);Con([])])]);Con([])]);
Con([Con([Num(3);Con([Con([Num(9);Num(1);Num(8);Num(9)])]);Num(6)]);Con([])]);
Con([Con([Num(0);Num(3);Con([Num(1);Con([]);Num(3);Con([Num(6);Num(7);Num(0)])]);Con([Num(2);Num(0);Con([Num(8);Num(8);Num(0);Num(5);Num(2)]);Num(8)])]);Con([])]);
Con([Con([Num(7);Num(5)]);Con([Con([Con([Num(6);Num(6);Num(1)])]);Num(5);Num(9);Num(1);Num(4)]);Con([Con([Con([Num(8);Num(8);Num(8)]);Num(1)]);Num(9);Con([Con([Num(4);Num(9)]);Num(3);Num(2);Num(0);Con([Num(0);Num(4)])])])]);
Con([Con([Num(4);Con([])]);Con([Num(4);Con([Con([Num(9);Num(4);Num(7)])]);Num(1);Con([Con([Num(3)]);Con([Num(4);Num(6);Num(8)]);Con([Num(8);Num(8);Num(9)])]);Num(8)]);Con([Con([Con([Num(4)]);Num(9);Con([Num(0);Num(1);Num(4)])]);Con([Con([Num(10);Num(6);Num(2)]);Num(10);Num(8);Num(0);Con([Num(1);Num(3);Num(1);Num(3);Num(8)])])]);Con([Con([Num(3);Con([Num(6);Num(3);Num(10);Num(10)]);Con([])]);Con([Num(7)]);Num(2)]);Con([Num(10);Num(9);Num(2);Num(0);Num(7)])]);
Con([Con([Num(9);Num(0);Con([Con([]);Num(6);Con([Num(4)]);Num(2);Num(9)]);Num(2)]);Con([Con([Con([Num(6);Num(2);Num(10);Num(1);Num(7)]);Con([Num(10)]);Con([]);Num(9)]);Con([Con([Num(3);Num(5);Num(6);Num(2);Num(6)])]);Num(7)]);Con([Num(3);Con([Con([Num(7);Num(6)]);Num(9)])]);Con([Con([Con([Num(2);Num(0);Num(9);Num(2)]);Num(3);Con([Num(10);Num(4);Num(7);Num(9)]);Con([Num(3)])]);Num(4);Con([]);Con([Con([Num(6);Num(2)]);Num(5);Num(0);Num(5);Num(4)])])]);
Con([Con([Con([Num(2);Con([Num(2);Num(9)])])]);Con([Con([]);Con([Con([Num(3);Num(9);Num(1);Num(1);Num(3)])]);Num(6);Num(6);Con([])])]);
Con([Con([Num(0);Num(6);Con([Con([Num(8);Num(8);Num(4);Num(8);Num(10)]);Num(9)])]);Con([Num(3);Con([Num(7);Num(9);Num(6);Con([Num(5)])]);Con([Num(7);Num(5);Con([Num(5)])]);Num(8);Num(9)])]);
Con([Con([Num(5);Num(2);Con([Con([]);Num(4);Con([]);Con([Num(4);Num(10);Num(1);Num(10)]);Num(10)]);Num(3);Con([Con([Num(0);Num(7);Num(5);Num(2);Num(0)]);Num(4);Con([Num(5);Num(5);Num(5);Num(0)])])]);Con([Con([Con([Num(2);Num(6);Num(4)]);Num(8)]);Con([Num(6);Num(3);Con([Num(8);Num(10);Num(1);Num(5)]);Num(9);Con([Num(9);Num(7)])]);Num(8);Con([Num(8);Num(5);Num(1);Num(4);Num(1)]);Con([Con([Num(0);Num(3);Num(2);Num(2)])])])]);
Con([Con([Num(10)]);Con([Num(2);Con([Num(2)]);Con([Num(8);Con([Num(3);Num(3)]);Num(6)]);Num(8)]);Con([Con([Con([Num(0);Num(9);Num(5);Num(9);Num(5)])]);Num(2);Num(6)])]);
Con([Con([Con([]);Con([Con([Num(3);Num(4);Num(4)]);Con([Num(1);Num(10);Num(10);Num(0);Num(7)]);Con([Num(10);Num(5);Num(5)]);Con([Num(2);Num(6);Num(0);Num(0)])]);Num(7);Num(2);Num(5)]);Con([Num(8);Con([Con([Num(5);Num(0);Num(6)])]);Num(1);Num(10);Con([Num(4);Con([Num(7);Num(3);Num(6)]);Con([Num(3);Num(10);Num(5)]);Num(4);Con([Num(4);Num(2);Num(5);Num(9)])])]);Con([]);Con([Num(8);Num(7);Num(0);Con([Num(7);Con([Num(2);Num(5);Num(5)]);Num(10);Num(8);Num(9)])]);Con([Con([Con([Num(0)]);Num(10);Num(1);Con([Num(7)])]);Num(10);Num(7)])]);
Con([Con([]);Con([Con([Con([Num(7);Num(4);Num(1);Num(1)]);Con([]);Num(5);Num(10)]);Con([Num(8);Con([Num(9);Num(1);Num(4);Num(6);Num(5)]);Con([Num(3);Num(7);Num(5);Num(2);Num(8)]);Num(0)]);Con([Con([Num(4)]);Num(10);Con([Num(5);Num(5);Num(6);Num(8)]);Con([Num(3);Num(2);Num(7)])]);Num(4)])]);
Con([Con([Con([]);Con([Con([Num(0);Num(1);Num(1);Num(8)]);Con([]);Con([Num(7)]);Con([Num(10);Num(3);Num(4);Num(8);Num(8)])]);Num(0)]);Con([Con([Num(1)]);Num(0);Con([Con([Num(6);Num(2);Num(10);Num(2);Num(4)]);Con([Num(7)]);Con([Num(10)])]);Con([]);Num(5)]);Con([]);Con([Con([Num(10);Con([Num(0);Num(6);Num(10)]);Num(5);Con([Num(1);Num(9);Num(3)])])])]);
Con([Con([Con([Con([]);Num(1);Num(0);Num(3);Num(10)]);Num(8)]);Con([Num(0);Con([Num(8);Num(4);Num(6);Num(4);Num(10)]);Num(7);Con([Num(6);Num(4);Con([Num(3);Num(1);Num(7);Num(3);Num(3)]);Num(9)]);Num(10)]);Con([Num(9);Con([Con([Num(7);Num(2);Num(9)]);Num(1);Con([Num(7);Num(7);Num(7)]);Con([])]);Num(10);Con([Con([Num(5);Num(5)])]);Num(5)]);Con([Num(7);Con([]);Con([Num(0);Num(8);Con([Num(2);Num(4)])]);Num(4)]);Con([Num(0);Con([Con([Num(6);Num(7);Num(8)]);Num(5)]);Con([Num(10)]);Num(9);Num(8)])]);
Con([Con([Con([])]);Con([Num(7);Con([Con([Num(10)]);Con([Num(1);Num(3);Num(8);Num(6)]);Con([]);Num(0);Num(3)])])]);
Con([Con([]);Con([Num(8);Num(7);Num(7);Con([Num(7)]);Con([Con([]);Con([Num(4);Num(4)]);Con([Num(2);Num(1);Num(9);Num(0)])])]);Con([])]);
Con([Con([Con([Con([Num(0);Num(3);Num(4);Num(0)])]);Num(4);Con([Con([Num(8)])])])]);
Con([Con([Num(5);Con([])]);Con([Num(6)]);Con([])]);
Con([Con([Con([Num(1);Con([]);Con([Num(10);Num(5);Num(9)])]);Con([Con([Num(0)]);Con([Num(2);Num(10);Num(0);Num(1);Num(9)])]);Num(6);Num(6);Con([Con([Num(4);Num(6)]);Con([Num(6);Num(0);Num(9)]);Num(0)])]);Con([Con([Num(6);Num(4);Con([Num(0);Num(5);Num(4);Num(5)]);Con([])]);Con([Num(4);Num(6);Con([Num(2);Num(2);Num(0)])])]);Con([Con([Con([]);Num(5);Con([Num(6);Num(8);Num(9)]);Con([Num(6);Num(6)]);Con([Num(9);Num(3)])]);Con([Num(3);Con([Num(6)]);Num(4);Num(4)])]);Con([Num(10);Num(5);Num(8);Con([Con([Num(1);Num(8);Num(8)]);Con([Num(6)])])])]);
Con([Con([Num(2);Num(10);Con([Con([Num(3)]);Con([Num(8);Num(3)]);Con([Num(7);Num(0)]);Con([])]);Con([Con([Num(1);Num(7);Num(3);Num(9)]);Num(9);Con([Num(2);Num(10);Num(1);Num(9);Num(10)]);Num(10);Con([Num(5)])])]);Con([Num(2);Num(10);Num(7);Num(7);Num(8)])]);
Con([Con([Con([Num(7);Num(4)]);Con([Con([]);Num(5)])]);Con([Num(1)])]);
Con([Con([Con([Con([Num(9)])]);Num(4);Num(7)]);Con([Con([]);Num(4);Num(2);Num(0)]);Con([]);Con([Num(10);Con([Con([]);Num(0)])]);Con([Num(7);Num(4);Num(0);Num(7)])]);
Con([Con([Num(7);Num(6);Num(8);Num(0)])]);
Con([Con([Num(6)]);Con([Con([Num(4);Con([Num(1);Num(8);Num(7);Num(8)]);Con([Num(9);Num(5)]);Con([Num(7);Num(9);Num(3)])])]);Con([Num(1);Con([Num(2);Num(1);Num(1);Con([Num(0);Num(3);Num(2);Num(9)])])]);Con([Con([Num(8);Num(7)])])]);
Con([Con([Con([]);Con([Num(5);Con([Num(6);Num(6)]);Con([Num(5);Num(3);Num(6)]);Con([])])]);Con([Con([Num(1);Num(3);Num(2)])]);Con([Con([Num(4)]);Num(1);Num(10)])]);
Con([Con([Con([Num(5)]);Num(1);Con([]);Con([Num(8);Num(2);Num(1)])]);Con([Con([Num(2);Num(6);Con([Num(7);Num(7)]);Con([Num(6);Num(4);Num(0);Num(8)]);Con([Num(8);Num(6)])]);Con([])]);Con([]);Con([Con([Num(5);Num(8)]);Num(0);Num(2)])]);
Con([Con([Con([Con([Num(8);Num(2);Num(10)]);Num(2)])]);Con([Con([Num(1);Num(0);Con([Num(2);Num(7);Num(6);Num(1)])]);Num(9);Con([Con([]);Con([Num(6)]);Num(1);Con([])]);Con([Con([])])]);Con([]);Con([]);Con([Con([Con([Num(5);Num(10);Num(6)])])])]);
Con([Con([Num(10)]);Con([Num(3)]);Con([Con([]);Con([Num(3);Con([]);Num(1)]);Con([Num(4);Num(5);Num(9);Num(8);Con([])])]);Con([Con([Con([Num(6)]);Num(6);Num(4);Num(0)]);Num(9);Con([]);Con([Num(8);Con([Num(1)]);Num(2)]);Num(5)])]);
Con([Con([Num(8);Num(0)]);Con([Con([Num(5);Con([]);Num(9)]);Num(8);Num(9);Num(9)]);Con([])]);
Con([Con([Num(10);Num(7);Num(2)])]);
Con([Con([]);Con([Num(7);Con([Con([]);Con([Num(8)]);Num(3);Num(0);Con([Num(1);Num(0);Num(7)])]);Con([Num(7)]);Con([Num(10);Num(7);Num(10);Num(0);Con([Num(8);Num(9);Num(4);Num(6);Num(0)])])]);Con([]);Con([])]);
Con([Con([Num(9);Con([Num(8)]);Con([Num(6)]);Con([Con([Num(10);Num(8);Num(4);Num(2);Num(7)]);Con([Num(6);Num(2)]);Con([Num(5);Num(6);Num(0)]);Con([])]);Num(6)]);Con([Con([Num(0)])])]);
Con([Num(0);Num(0);Num(0);Num(4)]);
Con([Num(0);Num(0);Num(0);Num(4);Num(4)]);
Con([Con([Con([]);Con([Con([Num(5);Num(4)]);Num(10);Con([Num(9)]);Con([Num(7);Num(4);Num(7);Num(1)])]);Num(5);Num(2);Con([])]);Con([Num(3);Con([Con([Num(5);Num(0);Num(5)]);Con([]);Con([Num(9);Num(3);Num(9)])]);Con([Con([Num(4);Num(10);Num(10)]);Num(0);Con([Num(9);Num(5);Num(8);Num(1);Num(6)])])]);Con([Num(6)])]);
Con([Con([Con([Con([]);Num(3);Num(5);Num(5)]);Con([Con([Num(9);Num(7);Num(3);Num(1);Num(7)])]);Num(8);Num(8)]);Con([Con([Num(4);Num(7);Num(0);Con([Num(8);Num(4)]);Num(4)]);Con([Con([Num(4);Num(9)]);Num(10);Num(0);Num(0);Con([Num(10);Num(4);Num(1);Num(0);Num(7)])]);Con([Con([Num(5)])]);Num(5)]);Con([Num(2);Num(6);Con([Num(7);Con([Num(2);Num(1)]);Con([Num(2);Num(6);Num(6);Num(4)]);Num(3);Con([Num(3);Num(0);Num(8);Num(10);Num(3)])]);Num(6)]);Con([Num(5);Num(7)]);Con([Con([Num(6);Con([Num(8);Num(3);Num(7);Num(2)]);Con([Num(5);Num(3);Num(6);Num(4);Num(10)]);Con([Num(2);Num(9);Num(0);Num(0);Num(5)]);Num(4)]);Con([]);Num(5)])]);
Con([Con([Con([Num(2);Con([Num(0);Num(5);Num(10);Num(5)]);Con([Num(4);Num(3);Num(6);Num(9);Num(1)]);Con([Num(7);Num(0)]);Num(3)]);Con([Num(0);Num(4);Con([Num(2);Num(4);Num(10)]);Num(8)]);Con([Con([Num(4);Num(10);Num(1);Num(9)])])]);Con([]);Con([Con([Con([]);Num(6);Num(3);Num(7)])])]);
Con([Con([Num(7)]);Con([Con([]);Num(7);Num(1)])]);
Con([Con([Num(5);Con([])]);Con([]);Con([Num(3);Con([Con([])])]);Con([])]);
Con([Con([Con([]);Con([Con([Num(3);Num(1);Num(5)])])]);Con([]);Con([Num(4);Num(3)]);Con([Num(1);Con([Num(8);Con([Num(5);Num(5)]);Con([]);Con([])]);Con([Num(8);Num(0);Con([Num(3)])]);Con([Num(6);Con([Num(0);Num(8);Num(3);Num(0);Num(1)]);Con([Num(2);Num(1);Num(7);Num(6)]);Con([Num(8);Num(9);Num(6);Num(1)])])]);Con([Con([Num(1);Con([])])])]);
Con([Con([Con([Num(8)]);Num(6);Con([Num(2);Con([Num(6);Num(10);Num(2)]);Num(9);Num(5)]);Con([Con([Num(0);Num(7)]);Num(1);Con([Num(0);Num(0);Num(1);Num(8)]);Con([Num(7)]);Con([])]);Con([Num(0)])]);Con([Num(7)]);Con([Num(1);Num(8);Con([]);Con([]);Con([Con([Num(4);Num(2);Num(7)]);Num(8);Num(10)])]);Con([Con([Num(8);Con([Num(6);Num(10);Num(5);Num(0);Num(4)]);Con([Num(8);Num(3);Num(8)]);Con([Num(0);Num(5);Num(4);Num(10);Num(0)]);Num(2)]);Con([Num(4)]);Con([Con([]);Num(5);Con([Num(3);Num(6);Num(9);Num(8);Num(0)]);Con([Num(7);Num(9);Num(6)]);Con([Num(0);Num(4);Num(2);Num(5);Num(9)])])]);Con([Con([Num(4)]);Num(3)])]);
Con([Con([Num(2);Con([Con([Num(0);Num(7);Num(0)]);Num(2);Con([Num(8);Num(9);Num(8);Num(5)]);Con([Num(4);Num(1);Num(7);Num(0)]);Num(9)]);Num(6);Con([Num(5);Num(2)]);Num(5)]);Con([]);Con([Num(4);Num(0)]);Con([Num(1)]);Con([Num(9);Con([Con([Num(7)]);Num(6);Con([Num(0)]);Con([Num(3);Num(3);Num(9)])]);Num(6);Con([])])]);
Con([Con([Num(8);Num(2);Num(2);Num(1)]);Con([]);Con([Num(5)]);Con([Con([Num(5);Con([Num(3);Num(9);Num(7);Num(10);Num(1)])])]);Con([Con([]);Num(9)])]);
Con([Con([Num(0);Num(1);Num(4);Num(4)]);Con([Con([Num(3);Con([Num(9);Num(6)])]);Con([]);Num(10)])]);
Con([Con([Num(6);Con([Con([Num(0);Num(8);Num(0)]);Num(9);Con([Num(0);Num(10);Num(6);Num(2);Num(7)])]);Con([Num(6);Num(6);Num(9);Num(7);Num(9)]);Num(7);Num(10)])]);
Con([Con([Con([Num(3);Con([])]);Con([Con([Num(8);Num(0);Num(8)]);Con([Num(9)])])])]);
Con([Con([Num(1);Num(9);Con([Con([Num(0)])]);Num(4);Con([Num(7);Num(5)])]);Con([Con([Con([Num(6)]);Con([Num(8)]);Con([]);Num(10)])]);Con([Con([Num(0);Num(1);Num(1);Num(0);Con([Num(9)])]);Num(4);Num(3);Num(0)]);Con([Num(5);Num(10)]);Con([Num(8);Num(9)])]);
Con([Con([Num(3);Num(5);Con([Con([Num(3);Num(6);Num(7)]);Con([Num(2);Num(6)]);Num(3);Num(3)])]);Con([Num(8);Num(7)])]);
Con([Con([Con([Num(8);Num(9)]);Num(2);Num(9)]);Con([]);Con([Con([Num(6);Con([Num(8);Num(1);Num(0)]);Num(4);Con([Num(3);Num(2);Num(1);Num(4)])]);Con([Num(1);Num(4);Con([Num(9)]);Num(2);Con([Num(1);Num(1);Num(8);Num(10);Num(9)])]);Num(0);Num(6)]);Con([Num(8);Num(9);Con([]);Num(8)])]);
Con([Con([Num(9)]);Con([])]);
Con([Con([Num(10);Con([]);Con([Con([Num(5);Num(7);Num(7)]);Con([Num(3);Num(8);Num(2);Num(2)])]);Num(1)])]);
Con([Con([Num(2);Num(4);Num(10)]);Con([Num(0);Num(1);Num(3);Con([])])]);
Con([Con([Num(1);Con([Con([]);Num(6);Con([Num(7);Num(7)]);Num(9);Num(2)]);Num(6)]);Con([Con([Num(3)])]);Con([])]);
Con([Con([]);Con([Con([Con([Num(9);Num(8);Num(2)]);Num(9);Num(5)]);Num(4)])]);
Con([Con([Con([Con([Num(3)])])]);Con([Con([Num(5);Num(0);Num(9);Num(7)]);Con([Num(5)]);Num(6)]);Con([Num(6)])]);
Con([Con([Con([Con([]);Num(9);Num(8);Num(6);Num(2)])]);Con([]);Con([Num(3);Con([Num(10)]);Num(9)])]);
Con([Con([Num(2);Con([Con([Num(8);Num(1);Num(7);Num(4)]);Num(4);Num(9)]);Con([Num(8);Con([Num(3)]);Con([Num(1);Num(5);Num(2)])])]);Con([]);Con([Num(4);Num(5);Con([Con([]);Con([Num(10)]);Con([Num(1);Num(4);Num(2);Num(7)])])]);Con([Num(7)])]);
Con([Con([]);Con([Con([Con([Num(8);Num(4);Num(1);Num(3)])]);Con([Num(3);Con([Num(4);Num(3)]);Con([Num(2);Num(7);Num(1)]);Num(4);Num(0)]);Con([])]);Con([Con([Con([Num(7);Num(0);Num(8);Num(2)]);Con([Num(5)])]);Con([]);Con([Con([Num(2)]);Num(8)]);Con([Num(0);Con([Num(10);Num(6);Num(2);Num(1);Num(3)]);Con([Num(8);Num(8);Num(7);Num(3);Num(3)]);Con([])]);Num(8)])]);
Con([Con([Num(2);Num(7);Num(7);Num(4);Num(1)]);Con([Num(9);Con([]);Num(1);Num(4)]);Con([Con([Num(9);Con([])]);Num(5)])]);
Con([Con([Num(2);Num(8);Con([Con([]);Con([Num(0);Num(6);Num(10);Num(10);Num(2)]);Con([])]);Con([Num(8);Num(3);Num(10);Con([Num(5)]);Con([Num(4);Num(8);Num(4);Num(7)])]);Num(0)]);Con([]);Con([Con([Num(8);Con([Num(2)]);Num(8)]);Num(5);Con([Con([Num(2);Num(3);Num(1);Num(8);Num(0)])]);Num(6);Num(5)])]);
Con([Con([Num(6);Num(1);Num(10);Con([Num(0);Num(8);Con([Num(2);Num(6);Num(1)]);Num(0)]);Con([Num(8);Con([Num(6);Num(5);Num(3)]);Num(4)])])]);
Con([Con([]);Con([Num(10);Con([Con([Num(0)]);Num(1)]);Num(2);Num(8)]);Con([Num(9);Num(2)]);Con([Con([Con([Num(7);Num(0)]);Num(8);Num(6)]);Num(9);Con([Num(7);Con([Num(4)])]);Con([Num(6);Num(9)])])]);
Con([Con([Num(5);Num(9);Con([Con([Num(1)]);Con([]);Num(8);Num(0);Num(9)])]);Con([]);Con([]);Con([Con([Num(1);Con([Num(4);Num(9);Num(5);Num(5)]);Num(0)]);Num(2);Num(5);Num(4)]);Con([Num(1)])]);
Con([Con([Num(10)]);Con([Num(7);Num(9);Con([Num(6);Num(4)]);Num(9)]);Con([Num(2);Num(2);Num(5)]);Con([Num(5);Con([Num(9);Con([Num(9);Num(0);Num(8)]);Num(3)]);Num(4)]);Con([Num(2);Num(10);Num(8)])]);
Con([Con([]);Con([Con([Num(7);Num(4);Num(4);Con([Num(7);Num(1);Num(10);Num(6)]);Con([Num(8);Num(8)])]);Con([Con([Num(1);Num(6);Num(9);Num(3)])]);Num(9);Con([Con([Num(7);Num(9)]);Con([Num(8);Num(7);Num(3)]);Con([Num(0);Num(4);Num(6);Num(7)]);Num(9);Con([Num(10);Num(0)])]);Con([Num(8);Con([Num(9);Num(3);Num(10);Num(7)]);Num(7)])]);Con([Con([]);Con([]);Con([Num(6);Con([Num(0);Num(4);Num(8)]);Num(3);Con([Num(3);Num(2);Num(4);Num(0);Num(5)]);Con([Num(1)])]);Num(7);Con([Num(1);Num(4);Num(8)])])]);
Con([Con([Con([Con([Num(3)]);Con([Num(10);Num(0);Num(4);Num(3)]);Con([Num(3)]);Con([Num(1);Num(9);Num(0)])]);Num(5);Num(9)]);Con([Con([Num(3);Num(10);Num(1);Con([Num(9);Num(1);Num(4);Num(3)]);Num(10)]);Num(6);Con([])]);Con([Con([Num(0);Num(3);Con([Num(6);Num(3)]);Con([Num(3);Num(3);Num(2);Num(6);Num(9)]);Num(2)]);Num(7);Con([Num(7)]);Con([Num(3);Num(2);Num(2);Num(8)])]);Con([Num(8);Con([Num(5)]);Con([Con([]);Num(7);Con([Num(7);Num(5)]);Con([Num(0);Num(2);Num(1);Num(9);Num(6)])])]);Con([Con([Num(5);Con([Num(9);Num(7);Num(8);Num(6);Num(8)]);Con([Num(4)]);Con([Num(7);Num(5)]);Con([Num(1)])]);Con([Num(3);Num(6)])])]);
Con([Con([Con([Con([Num(0)]);Con([Num(4);Num(8);Num(8)]);Con([Num(7);Num(4);Num(4)]);Num(0);Num(10)]);Num(3)]);Con([Num(7)]);Con([])]);
Con([Con([Num(9);Con([Num(9);Con([Num(0);Num(9);Num(10)]);Con([Num(3)])]);Num(0);Num(5);Num(8)]);Con([])]);
Con([Con([Num(9);Con([Con([])]);Con([Con([Num(0);Num(3);Num(4);Num(4)]);Num(0)]);Con([Con([]);Con([Num(9);Num(4);Num(0);Num(7)]);Con([]);Num(4)])]);Con([]);Con([Con([]);Con([Num(1);Num(0);Num(9);Con([Num(1);Num(7);Num(1);Num(3);Num(0)])]);Con([Con([Num(1);Num(10);Num(4);Num(2);Num(1)])]);Num(7);Con([Con([Num(8);Num(6);Num(8);Num(0)]);Con([])])]);Con([Con([Num(10)])])]);
Con([Con([Num(4);Con([Con([Num(0)]);Num(10);Con([Num(5);Num(10);Num(3)]);Con([])]);Num(9);Num(6)])]);
Con([Con([Num(4);Num(6);Num(3);Con([]);Num(6)])]);
Con([Con([Num(8);Num(6);Num(9)])]);
Con([Con([Num(3);Con([Con([Num(6);Num(6)]);Con([Num(6);Num(8);Num(9)]);Num(5);Con([]);Con([Num(4);Num(1);Num(5)])]);Con([Con([Num(10);Num(7);Num(7);Num(0)]);Con([]);Con([Num(9);Num(10);Num(8);Num(3)]);Num(0);Con([Num(10);Num(3);Num(8);Num(6);Num(6)])]);Con([]);Num(3)]);Con([Con([Num(8);Con([]);Con([Num(3);Num(9);Num(2);Num(4)]);Con([Num(0);Num(1);Num(7);Num(6);Num(4)]);Num(4)]);Con([]);Num(7);Con([Con([Num(10);Num(8)]);Num(9);Con([]);Con([Num(9);Num(6);Num(5)])])]);Con([]);Con([Num(3);Num(9);Num(10)])]);
Con([Con([Con([Num(4);Num(6);Con([Num(7)])]);Con([Num(2);Num(4);Con([Num(9);Num(2);Num(8);Num(9);Num(3)]);Con([Num(7);Num(6)]);Con([Num(4);Num(3);Num(6);Num(1);Num(0)])]);Con([])])]);
Con([Con([Con([]);Num(2);Num(1)]);Con([]);Con([Con([Num(9);Con([Num(3);Num(1);Num(5);Num(6)]);Con([]);Con([Num(9);Num(7);Num(5)]);Con([Num(0);Num(0);Num(4)])])]);Con([Num(5);Num(2);Con([Con([Num(10);Num(10);Num(0)])])])]);
Con([Con([Con([Con([]);Con([Num(0);Num(8)]);Con([])]);Num(2);Con([Num(2);Num(3);Num(6);Num(0);Num(10)])]);Con([Num(8);Num(6);Con([Con([]);Con([Num(0)]);Con([Num(10);Num(8)]);Con([Num(1);Num(9);Num(4);Num(9)])])]);Con([Num(5);Con([]);Num(8);Con([Num(9);Num(4)])]);Con([Con([Con([Num(9);Num(7)]);Con([Num(7)]);Num(10)]);Num(6);Con([Con([Num(6);Num(1);Num(1);Num(6)]);Num(4);Num(2);Con([Num(6);Num(2);Num(4);Num(9);Num(3)])])])]);
Con([Con([Num(7)]);Con([Num(6);Num(5);Con([]);Num(9);Num(6)]);Con([Con([])]);Con([Num(5);Con([])])]);
Con([Con([Num(6);Num(9);Con([Num(4)]);Num(10)])]);
Con([Con([Num(8);Num(0)])]);
Con([Con([Num(2);Num(5);Num(5)]);Con([Num(5);Con([Num(7);Num(2);Num(6);Con([]);Con([Num(3);Num(3)])])]);Con([]);Con([Con([Num(10);Num(0);Con([])]);Num(5);Con([Con([Num(0);Num(6)]);Num(8);Num(3);Num(5);Num(9)]);Num(5)])]);
Con([Con([Num(1);Con([Con([Num(3)])]);Con([Con([Num(0);Num(0)]);Con([Num(2);Num(6)]);Num(8);Con([Num(2);Num(10)]);Con([Num(3);Num(9);Num(7)])]);Num(3);Num(2)]);Con([]);Con([]);Con([Num(5);Con([Num(8)]);Num(7);Num(0)]);Con([Num(4);Con([Con([]);Con([Num(2);Num(8);Num(0);Num(4)]);Con([Num(1);Num(9);Num(4)]);Con([]);Con([Num(10)])]);Con([Con([]);Con([Num(8)]);Con([Num(0);Num(1)]);Num(7)]);Con([Num(6);Con([Num(2)]);Num(9)])])]);
Con([Con([Num(10);Con([Num(0);Num(6);Con([Num(1);Num(3)])])])]);
Con([Con([Con([Num(8);Con([Num(9);Num(9);Num(7);Num(2)])])]);Con([Num(2)]);Con([Num(0);Con([Con([Num(6)]);Num(5);Con([Num(4);Num(0)])]);Con([Num(4);Con([Num(1);Num(4);Num(5)]);Con([])]);Num(10);Con([Con([]);Num(3);Con([Num(0);Num(1);Num(6);Num(8)])])]);Con([Con([Con([Num(6);Num(4)]);Num(1);Num(3);Con([])]);Num(3);Num(2);Num(5)])]);
Con([Con([Num(7);Con([Num(0);Con([Num(7);Num(1);Num(4)])])])]);
Con([Con([Con([Num(6);Num(9);Con([Num(5)])]);Num(8);Con([Con([Num(6);Num(2);Num(1);Num(2)]);Num(8);Num(6);Num(7);Num(1)]);Con([Con([Num(5);Num(9)]);Con([]);Con([Num(5);Num(6);Num(0)]);Con([])])]);Con([Num(0);Num(6);Con([Con([]);Num(9);Con([Num(2);Num(4)]);Con([Num(0)])])]);Con([]);Con([Con([Num(2);Con([Num(4);Num(7)]);Num(9);Num(8);Con([Num(8);Num(10);Num(1);Num(8)])])]);Con([Num(10);Num(4)])]);
Con([Con([Con([]);Con([])])]);
Con([Con([]);Con([]);Con([Con([Con([Num(0);Num(0)])]);Con([Num(0);Num(6)])]);Con([Num(10);Con([])])]);
Con([Con([Num(0);Num(4);Num(6);Num(7)])]);
Con([Con([Con([Con([]);Num(3);Con([Num(0);Num(10)]);Num(10)])]);Con([Num(7);Con([]);Con([Con([Num(4)]);Num(8);Num(2);Num(7)]);Con([])]);Con([])]);
Con([Con([Con([]);Con([]);Num(8);Num(1);Num(0)]);Con([Num(7);Num(1);Con([Con([Num(4);Num(1)]);Con([])]);Con([Con([Num(0);Num(0)]);Con([]);Con([Num(0);Num(5);Num(6)]);Con([Num(6);Num(9);Num(6);Num(9)]);Con([Num(6);Num(0);Num(2);Num(10)])]);Num(9)]);Con([Con([Con([Num(3)])])])]);
Con([Con([Con([])]);Con([]);Con([Con([Num(4);Con([]);Con([Num(3);Num(3);Num(10);Num(1)])]);Num(2);Con([Con([Num(0);Num(9)]);Con([]);Num(7);Con([Num(4);Num(1);Num(6);Num(6)])]);Con([Con([]);Con([]);Con([]);Con([Num(6);Num(0);Num(9)]);Con([Num(9);Num(8);Num(9);Num(10);Num(5)])])]);Con([Num(7);Con([Num(5);Con([Num(2)]);Num(6);Con([Num(8);Num(5);Num(8)]);Con([Num(2)])]);Con([]);Con([]);Con([Con([Num(5);Num(10);Num(10);Num(7);Num(8)]);Con([Num(8);Num(0)]);Con([Num(8)]);Num(0)])])]);
Con([Con([Con([Con([Num(8);Num(1);Num(8);Num(8)])]);Num(6);Num(5);Con([Con([Num(8);Num(0);Num(1);Num(2);Num(5)]);Con([Num(7);Num(5);Num(7);Num(1)]);Con([Num(9);Num(2);Num(2);Num(9)]);Num(1)]);Con([Num(0);Con([Num(4);Num(5);Num(9)]);Con([]);Num(8);Num(2)])]);Con([]);Con([])]);
Con([Con([Num(0);Con([Con([Num(4);Num(4);Num(7);Num(2)]);Con([Num(7);Num(8);Num(9);Num(7)]);Con([Num(0);Num(4);Num(1)])]);Con([Num(2);Con([Num(9)]);Num(7)]);Con([Num(2);Con([]);Con([]);Con([Num(1);Num(5);Num(5)])]);Num(5)]);Con([Num(0);Num(8);Con([Num(4);Num(3);Con([]);Con([Num(5);Num(2);Num(9)])])]);Con([Num(8);Con([Num(1);Con([Num(0);Num(0);Num(5);Num(6)]);Num(1);Num(4);Num(1)]);Con([Num(7);Con([Num(1);Num(9)]);Con([Num(2)]);Con([Num(9);Num(0);Num(8);Num(0)])]);Num(3);Con([Num(0);Num(1);Con([Num(0);Num(1);Num(1)])])])]);
Con([Con([Con([Con([Num(6)]);Con([Num(5);Num(4);Num(0);Num(6);Num(0)])]);Num(5)])]);
Con([Con([Con([Con([Num(9);Num(2);Num(4);Num(6);Num(0)]);Con([Num(9);Num(1)])])]);Con([Num(4);Con([Con([Num(8);Num(1);Num(10)]);Num(6);Con([Num(5);Num(9);Num(0);Num(7);Num(2)]);Num(9);Num(6)]);Num(2);Num(10);Num(2)]);Con([]);Con([]);Con([Con([Con([Num(4);Num(10);Num(10);Num(5)])]);Con([]);Con([Num(5)]);Con([Num(1);Num(3);Num(6);Num(2);Num(7)])])]);
Con([Con([Con([Con([Num(3);Num(7)])]);Con([Num(5);Con([]);Con([Num(4);Num(2);Num(5);Num(9)])]);Num(3);Con([])]);Con([]);Con([Con([Num(5);Num(1)]);Num(1)]);Con([Con([Con([Num(10);Num(6);Num(6);Num(8);Num(3)])])])]);
Con([Con([]);Con([Con([Num(8);Num(4);Con([Num(0);Num(10);Num(8)]);Con([])])]);Con([]);Con([Con([Con([Num(2);Num(2);Num(9);Num(7);Num(2)]);Con([Num(5)]);Num(7);Con([Num(10);Num(1);Num(3)]);Num(6)]);Num(0);Con([Num(0)]);Num(1);Num(2)]);Con([Con([Con([]);Num(8);Con([Num(8);Num(4);Num(4);Num(2);Num(7)]);Con([Num(0);Num(6);Num(2);Num(3);Num(2)]);Con([Num(8)])]);Con([Num(7);Num(10)]);Num(0);Num(5)])]);
Con([Con([]);Con([Con([Num(5);Con([]);Con([Num(1);Num(5);Num(3);Num(3)]);Con([Num(7);Num(3);Num(1);Num(0)]);Con([Num(3);Num(1);Num(3);Num(10);Num(8)])]);Num(4);Con([]);Num(9)]);Con([Num(5);Num(4);Con([Con([Num(9);Num(10)]);Con([]);Con([Num(10);Num(7);Num(0)]);Num(0);Con([])]);Num(7);Num(2)]);Con([Num(2)])]);
Con([Con([Num(9);Num(0)]);Con([Num(5);Con([Num(8);Num(6);Num(0);Num(6)]);Con([Num(2);Num(9)])]);Con([Con([Con([])])]);Con([Con([Num(2);Num(8)]);Num(3);Con([Con([Num(2)])])])]);
Con([Con([Num(5);Num(4);Con([Con([Num(9)]);Con([Num(3)])]);Num(4)]);Con([Con([Num(6);Num(4)]);Num(1);Con([Num(2);Num(4);Con([Num(2)])]);Con([Con([Num(8);Num(1);Num(7);Num(9);Num(2)]);Num(8);Con([])]);Con([Num(3);Con([])])])]);
Con([Con([Con([Num(5)]);Num(4)]);Con([Num(4);Con([Con([Num(1);Num(9);Num(3);Num(7);Num(9)]);Con([])]);Num(6);Con([Num(10);Con([Num(2);Num(3);Num(2);Num(8);Num(2)]);Con([Num(6);Num(0);Num(0)]);Num(7);Con([Num(2);Num(9);Num(9);Num(7);Num(2)])]);Num(5)])]);
Con([Con([Con([Num(3)]);Num(10);Con([Num(0);Num(3);Con([Num(8);Num(4);Num(3);Num(0)]);Num(5)]);Con([]);Con([Num(6)])]);Con([Con([Num(9);Con([Num(0)])]);Con([Con([Num(5);Num(0);Num(6);Num(8);Num(5)]);Con([]);Num(1);Con([Num(6)]);Num(0)]);Con([Num(10);Num(3)]);Num(6)])]);
Con([Con([Num(0);Con([Num(0)]);Con([Num(3);Num(6);Con([Num(10);Num(0);Num(8);Num(1)]);Con([Num(2);Num(1)]);Con([Num(3);Num(1);Num(6);Num(9)])]);Con([Num(2);Con([Num(5);Num(2);Num(8);Num(3);Num(3)]);Num(1);Con([Num(10);Num(4)]);Num(3)])]);Con([Num(1);Num(10)]);Con([Num(2);Num(4)])]);
Con([Con([Num(10);Con([Num(7);Con([Num(9);Num(3);Num(1);Num(10)]);Con([Num(9)]);Con([Num(0);Num(10);Num(5);Num(9)]);Num(0)])])]);
Con([Con([Num(5);Num(2);Con([Con([Num(8);Num(1);Num(0)]);Con([]);Con([]);Num(5)]);Num(2);Con([Num(9);Con([]);Num(1);Num(6);Con([])])]);Con([Con([Num(2)])]);Con([Num(0);Con([Con([Num(10);Num(0);Num(4)]);Num(9)]);Con([Con([Num(8);Num(4)]);Con([Num(0)]);Num(8)])]);Con([Con([Con([]);Con([Num(0);Num(3)]);Num(9);Num(6)]);Con([]);Num(1);Con([Num(5);Num(2);Num(1);Num(3);Num(4)]);Num(9)]);Con([Con([Con([Num(7);Num(4);Num(3);Num(5)]);Con([Num(3);Num(8);Num(5);Num(0)])]);Num(2);Num(2)])]);
Con([Con([]);Con([Num(10);Con([Num(9);Num(7);Con([Num(2);Num(5);Num(1);Num(9);Num(1)]);Con([Num(3);Num(0);Num(10)]);Num(0)])]);Con([Num(7);Num(1)]);Con([Num(9);Num(5);Num(1)])]);
Con([Con([Num(6)]);Con([Con([Num(4);Con([Num(2);Num(7);Num(5);Num(2);Num(8)])])]);Con([Num(3);Con([Num(5)]);Num(6);Num(1);Num(8)]);Con([])]);
Con([Con([Con([Num(8);Num(8);Con([])]);Num(3);Num(2)]);Con([Con([]);Con([Num(8);Num(0);Num(6);Con([])])]);Con([Num(10);Num(4);Con([]);Con([]);Con([Con([Num(2);Num(8);Num(3);Num(10);Num(3)]);Num(8);Num(3)])]);Con([Con([Con([Num(8);Num(6);Num(7)]);Num(3)]);Num(6)]);Con([Con([Con([Num(6)]);Con([Num(0);Num(4);Num(7)]);Con([Num(9);Num(1);Num(10);Num(10);Num(9)]);Con([Num(8);Num(5);Num(3)])]);Con([Con([Num(5);Num(7);Num(10);Num(7);Num(4)]);Num(9);Con([Num(1);Num(2)])]);Num(7);Con([]);Num(9)])]);
Con([Con([]);Con([Num(2)])]);
Con([Con([Con([]);Num(6);Num(8);Num(4)]);Con([Con([Con([Num(4);Num(9);Num(4);Num(9);Num(7)]);Num(2);Num(6)]);Con([Num(6);Num(0)]);Con([Num(6);Con([Num(0);Num(8)]);Num(3)]);Con([Num(1);Con([])])])]);
Con([Con([Num(4);Num(6);Con([Con([Num(6);Num(9);Num(8);Num(9)]);Num(1);Con([Num(2);Num(7);Num(9)])]);Num(2)]);Con([Num(8)])]);
Con([Con([Con([]);Con([Num(10);Con([Num(5);Num(8);Num(8);Num(7)]);Num(2)]);Num(1);Num(5)]);Con([Con([Con([]);Con([Num(7);Num(10)]);Con([Num(8);Num(8);Num(7)])])]);Con([Con([Num(9);Con([Num(6);Num(10);Num(7);Num(7);Num(10)]);Con([Num(7);Num(10);Num(10);Num(1);Num(0)]);Num(0);Num(4)]);Num(6);Num(10)])]);
Con([Con([Num(9)]);Con([Con([Num(10);Num(1)]);Num(7);Con([Num(6);Num(5);Con([Num(3);Num(8);Num(5);Num(7);Num(9)])]);Num(10);Con([Con([Num(6);Num(6);Num(6);Num(10)]);Num(0)])])]);
Con([Con([Num(7);Num(0);Num(8);Num(6)]);Con([Con([Con([Num(10);Num(4);Num(2);Num(4);Num(4)]);Num(0);Con([])]);Num(0)]);Con([Num(4);Con([Num(6);Num(5)]);Con([Num(3);Con([Num(7);Num(6);Num(7);Num(8);Num(10)]);Num(7)]);Num(5);Con([Con([Num(8);Num(6);Num(7);Num(10)]);Num(7);Con([Num(1);Num(0);Num(8)])])]);Con([Con([Con([Num(3)])]);Num(10)]);Con([Con([Con([Num(0)])]);Num(2)])]);
Con([Con([Num(5);Num(4);Num(6);Num(10)]);Con([Num(0)]);Con([Con([Con([Num(4);Num(4);Num(2);Num(0)])]);Num(1);Num(2)]);Con([Con([Con([Num(4);Num(7);Num(5)]);Con([]);Con([Num(7);Num(1);Num(2)]);Con([Num(2);Num(3)])]);Con([Num(10);Con([Num(3);Num(7);Num(3);Num(6);Num(0)]);Con([Num(10)]);Num(4);Num(5)])])]);
Con([Con([Num(8);Con([Num(7)]);Num(4);Con([Con([]);Num(3);Con([Num(4);Num(2);Num(1)]);Con([Num(1);Num(6);Num(3);Num(7)])]);Num(5)]);Con([Con([Con([Num(4);Num(5);Num(6)]);Con([Num(7);Num(7)]);Num(7)]);Num(2);Num(5);Num(10);Num(5)]);Con([]);Con([])]);
Con([Con([Con([Con([Num(3);Num(5);Num(0);Num(10)]);Num(0)]);Num(0);Con([]);Num(8)]);Con([Con([]);Num(0);Num(6);Con([]);Con([])])]);
Con([Con([Con([Con([Num(7);Num(5);Num(3)])]);Num(6);Num(2);Num(1)]);Con([]);Con([Num(7)]);Con([Num(3);Con([])])]);
Con([Con([Con([]);Num(6);Num(4);Num(9)]);Con([]);Con([Con([]);Num(5);Num(7)])]);
Con([Con([Con([Con([Num(8);Num(10);Num(10);Num(2);Num(4)]);Con([Num(7);Num(10);Num(10);Num(2)]);Num(0);Con([Num(3);Num(4)])]);Num(8);Con([]);Num(9)]);Con([Num(3);Con([Con([Num(8);Num(9);Num(8)]);Con([Num(2);Num(8);Num(2);Num(0);Num(2)])]);Con([Num(8);Num(9);Num(2)]);Num(1);Num(2)]);Con([Con([Con([Num(0);Num(10)])]);Con([Num(0);Num(4);Con([Num(0);Num(1);Num(1);Num(4);Num(4)]);Num(0)]);Num(9);Num(8);Con([Con([Num(10);Num(2);Num(0);Num(6)]);Con([]);Con([]);Con([Num(2);Num(3);Num(4);Num(9)])])]);Con([Num(4);Num(4);Con([Con([]);Num(8)]);Con([Num(5);Con([Num(10);Num(2);Num(4);Num(4);Num(8)]);Con([Num(0);Num(10)]);Con([Num(8);Num(3);Num(10);Num(2)]);Con([Num(8);Num(8);Num(7);Num(2)])]);Con([Num(1);Con([Num(7);Num(1);Num(0)]);Num(2);Con([])])])]);
Con([Con([Con([Con([]);Num(6);Con([Num(1);Num(0);Num(1);Num(0);Num(2)])]);Con([Num(3);Num(3);Con([Num(7);Num(10);Num(2);Num(4)]);Con([Num(2);Num(10);Num(10)]);Num(5)]);Con([Num(1)]);Con([Num(6);Con([Num(9);Num(3);Num(2);Num(9);Num(3)]);Con([Num(4);Num(8);Num(2);Num(1);Num(6)]);Con([Num(5);Num(6);Num(5)])])]);Con([Con([Con([Num(3);Num(6)]);Num(1)]);Con([Con([Num(8);Num(9);Num(3);Num(3);Num(7)]);Con([Num(10)]);Con([Num(1);Num(8);Num(0);Num(9)]);Con([Num(3);Num(6);Num(7);Num(10);Num(7)])])]);Con([Con([Num(0);Con([Num(6);Num(4);Num(2);Num(7)]);Num(8);Con([Num(7);Num(4)])]);Con([Con([Num(3);Num(2);Num(7)]);Con([Num(9);Num(1);Num(7)]);Num(1);Num(4)])]);Con([Con([Con([Num(4);Num(6);Num(7);Num(1);Num(1)])]);Con([Con([Num(2);Num(8);Num(8);Num(2)])]);Con([Con([]);Num(0);Con([Num(4);Num(3);Num(8);Num(3);Num(5)]);Num(10);Num(7)]);Num(10)]);Con([Con([Num(0);Num(8)])])]);
Con([Con([]);Con([Num(10);Con([Con([]);Num(6)])]);Con([Con([]);Num(6);Num(6);Num(8)]);Con([])]);
Con([Con([Num(7);Num(2)]);Con([Num(9)])]);
Con([Con([Con([Con([Num(10);Num(8);Num(6);Num(1);Num(6)]);Con([Num(9);Num(3)])]);Con([Con([Num(4);Num(9)]);Con([Num(3);Num(2)])]);Num(9)]);Con([Num(9);Num(8);Con([Con([Num(0);Num(9);Num(8)]);Con([]);Con([Num(2)]);Num(2)]);Con([Con([]);Con([Num(10);Num(3);Num(7);Num(1)])])]);Con([Num(1);Con([Con([Num(0);Num(7);Num(5);Num(3);Num(8)]);Con([Num(3);Num(2)]);Con([]);Num(10);Num(10)]);Num(1);Num(7);Con([Num(4);Con([Num(9);Num(3);Num(2);Num(5)]);Con([Num(1)]);Con([Num(5)]);Con([Num(4);Num(3);Num(5);Num(3);Num(7)])])])]);
Con([Con([]);Con([Num(1)]);Con([Num(3);Con([Con([Num(0);Num(10)])]);Con([Num(3)]);Con([Num(9);Con([]);Con([Num(7)])]);Num(6)]);Con([Num(10);Con([Num(1);Num(8);Con([Num(6)]);Con([])]);Num(4);Num(8)]);Con([Num(6)])]);
Con([Con([Con([Con([Num(6);Num(10)])])])]);
Con([Con([Num(10);Con([]);Num(10);Con([Num(7);Num(5)]);Con([Con([Num(8);Num(10);Num(3);Num(0);Num(10)]);Num(0)])])]);
Con([Con([]);Con([Con([Num(3);Num(6);Con([]);Con([Num(1);Num(8);Num(4)]);Con([])]);Con([Con([Num(8);Num(8);Num(6)]);Con([Num(3);Num(0);Num(0);Num(4);Num(5)]);Num(2);Con([Num(2);Num(8)]);Num(2)]);Num(6);Con([])]);Con([Num(4)])]);
Con([Con([Num(8);Con([Num(5)]);Con([Con([]);Con([Num(10);Num(10);Num(5);Num(4);Num(4)]);Con([Num(1);Num(7);Num(0);Num(7);Num(7)]);Num(9)]);Con([Con([Num(6);Num(10);Num(2)]);Num(9)]);Num(8)]);Con([Num(7);Con([Con([Num(0);Num(8);Num(1);Num(2);Num(10)]);Num(6);Con([]);Num(0)]);Num(9);Num(6)]);Con([Num(10)])]);
Con([Con([Num(2);Num(0);Con([Con([Num(9);Num(3);Num(2);Num(5);Num(2)]);Num(7);Con([Num(3);Num(8)])]);Num(8)]);Con([Num(1);Num(7)]);Con([Num(10);Num(6);Num(5);Con([])]);Con([Num(0);Con([Num(2);Con([Num(6);Num(2);Num(5)]);Con([])]);Num(9)])]);
Con([Con([Num(0)])]);
Con([Con([Con([Num(9)]);Num(10);Num(3);Con([Con([Num(1);Num(2);Num(3)]);Num(7);Num(4);Num(0)]);Num(6)]);Con([Con([Con([Num(2);Num(10);Num(9)]);Num(1);Con([])]);Num(6);Con([Con([]);Con([Num(5);Num(10);Num(0);Num(7)]);Con([Num(7);Num(0);Num(5)])]);Con([Con([]);Num(4)])]);Con([Num(4);Num(7);Con([Num(3);Num(2);Con([Num(3);Num(10);Num(9);Num(3)]);Num(9);Num(6)]);Con([Con([Num(2);Num(8);Num(3)]);Con([Num(10);Num(0);Num(3)]);Con([Num(4)]);Num(7);Con([Num(3);Num(3)])]);Con([Con([Num(0)]);Con([Num(6)])])]);Con([Con([Con([Num(1);Num(4);Num(3)]);Num(2);Con([Num(1);Num(7)]);Con([Num(6);Num(1);Num(7)]);Num(4)]);Num(3);Con([Con([Num(5);Num(8);Num(10);Num(10);Num(8)]);Num(6);Num(8);Con([Num(0);Num(0);Num(3)])]);Num(7)]);Con([Con([Con([Num(9);Num(2);Num(7)]);Num(7);Num(9);Con([Num(1);Num(1);Num(1);Num(1)]);Con([Num(4);Num(10);Num(0);Num(6);Num(0)])]);Num(5);Con([Num(1);Num(10);Con([Num(10);Num(8)])])])]);
Con([Con([Num(0);Num(1);Num(5);Num(1);Num(2)]);Con([Con([])]);Con([Con([Num(2);Con([Num(0);Num(2);Num(7);Num(10)]);Num(4);Num(5);Num(2)]);Con([Num(5)]);Con([Num(1);Con([Num(5);Num(1);Num(6);Num(6);Num(5)]);Con([Num(3)])])]);Con([Con([Num(6)]);Num(2);Num(8)])]);
Con([Con([Con([Con([Num(8);Num(1);Num(5);Num(10)]);Num(3);Num(10);Num(8)])]);Con([Num(1)]);Con([Con([Con([Num(7);Num(7);Num(7)]);Num(8)]);Con([Num(9);Con([Num(0);Num(4);Num(6);Num(10);Num(0)]);Num(4);Num(5)])]);Con([Con([Con([]);Num(8);Con([Num(5);Num(10);Num(8);Num(10)]);Num(0)]);Con([Con([]);Con([Num(5);Num(7)])])]);Con([Num(1);Con([Num(4);Num(0);Num(2);Con([Num(5);Num(8);Num(8);Num(7);Num(2)]);Con([Num(5);Num(7);Num(10);Num(9)])]);Con([]);Num(6)])]);
Con([Con([Con([Num(7);Con([Num(3)]);Con([Num(9);Num(1);Num(7);Num(4)])])]);Con([]);Con([Con([Con([Num(5)]);Num(1)]);Num(9)])]);
Con([Con([Con([])]);Con([Num(7)]);Con([Con([Con([])]);Num(7)])]);
Con([Con([Num(10);Con([Num(9);Con([Num(6);Num(2);Num(2);Num(10)]);Con([Num(6)]);Num(10)]);Con([Con([Num(6);Num(10);Num(0);Num(2);Num(8)])]);Con([Con([Num(10);Num(10)]);Num(8)]);Num(6)]);Con([Con([Con([Num(0);Num(1)]);Con([Num(2);Num(6);Num(7);Num(1)]);Con([Num(2)])])]);Con([Num(4);Con([Con([]);Con([Num(5);Num(9);Num(6);Num(0)]);Con([Num(7);Num(6)]);Con([Num(10);Num(10);Num(8);Num(2);Num(7)])])])]);
Con([Con([Con([Num(7);Con([Num(8);Num(4)])]);Con([Con([])]);Num(9);Con([Con([Num(9);Num(8)])])]);Con([Con([Num(2);Num(10);Num(9);Num(3)]);Num(7);Con([Num(7)])]);Con([Con([]);Num(4)]);Con([Num(2)]);Con([])]);
Con([Con([Num(2);Con([Num(2);Con([Num(3);Num(10);Num(0)])]);Con([]);Con([Con([Num(1);Num(1);Num(4);Num(3);Num(2)]);Num(6);Con([]);Num(7)])]);Con([Num(8);Num(0);Num(1)]);Con([Con([Con([Num(4);Num(1);Num(7)]);Num(6);Con([Num(0);Num(5);Num(5);Num(9);Num(0)]);Num(6)]);Num(3);Con([Num(3);Num(8)])])]);
Con([Con([Con([Num(5);Con([Num(1);Num(7);Num(8)]);Con([Num(0);Num(9)]);Num(6);Con([Num(4);Num(0)])]);Con([Con([Num(1)]);Con([Num(10);Num(6);Num(7)]);Con([Num(9);Num(2);Num(3)])]);Con([Con([Num(3);Num(5);Num(10);Num(8);Num(2)]);Num(5);Con([Num(1);Num(9);Num(7)]);Num(7)])]);Con([Num(10);Con([Con([])]);Con([Con([]);Num(0)]);Con([Con([Num(3);Num(3);Num(10)]);Con([Num(6);Num(6);Num(3);Num(10);Num(8)]);Con([]);Num(6);Con([Num(6);Num(6);Num(8);Num(1);Num(8)])]);Num(9)]);Con([Num(7);Num(8);Con([Con([Num(1);Num(0)]);Num(3);Num(8)]);Con([])]);Con([Con([])])]);
Con([Con([]);Con([Con([])]);Con([Num(6);Con([Con([Num(10);Num(1);Num(6);Num(3)])]);Con([Con([Num(9);Num(4);Num(7)])]);Con([]);Num(8)]);Con([Num(10);Con([Con([Num(0)]);Num(7);Con([Num(6);Num(0);Num(10);Num(7)])]);Con([Con([Num(6)]);Con([Num(4)])])]);Con([Con([Con([Num(5);Num(9);Num(4);Num(8)]);Con([Num(0)]);Con([Num(4);Num(10);Num(0)]);Con([Num(6)]);Con([])])])]);
Con([Con([Con([Num(0);Con([Num(5)]);Con([Num(7);Num(10);Num(9);Num(2)])]);Num(5);Con([Num(4);Num(10);Con([Num(0)]);Con([Num(5);Num(8);Num(2);Num(3)]);Num(2)]);Con([])]);Con([Num(2);Con([Num(7)])]);Con([Con([Num(4);Num(0);Con([Num(8);Num(2);Num(10);Num(2);Num(6)]);Con([Num(8);Num(7);Num(9)])]);Num(1);Num(10);Con([Con([Num(9);Num(6);Num(0);Num(3)]);Num(4);Num(0)]);Con([Con([Num(2);Num(10)]);Num(7);Con([Num(8);Num(5);Num(5)]);Con([Num(8);Num(1);Num(8)])])]);Con([Num(8)])]);
Con([Con([Con([Num(4);Con([Num(10);Num(1)])]);Num(4);Con([Num(3);Num(0)]);Num(4)])]);
Con([Con([Con([Num(5);Num(0);Num(5);Num(2)])])]);
Con([Con([Con([Num(6);Con([Num(4);Num(6);Num(5);Num(6);Num(3)])])]);Con([Con([Num(5)]);Num(7);Num(2);Num(10)]);Con([Con([Con([Num(0);Num(10)]);Con([Num(0);Num(4);Num(5);Num(4);Num(6)]);Num(1)]);Num(2);Con([Num(10);Num(5);Con([Num(7)]);Con([Num(5)]);Num(7)]);Num(7);Con([Num(7)])]);Con([Con([Num(8);Num(1);Num(9);Num(9);Num(1)]);Num(0)])]);
Con([Con([Num(1)])]);
Con([Con([Con([Num(7);Con([Num(1);Num(8);Num(1);Num(7);Num(2)])]);Con([]);Con([]);Con([Num(4);Num(2);Num(6);Con([Num(1);Num(7);Num(6)])])]);Con([Con([Num(3);Num(6);Con([Num(2);Num(1);Num(1)]);Con([])])]);Con([Con([Num(7);Con([]);Con([Num(2);Num(0);Num(0)]);Con([Num(9);Num(8);Num(6);Num(7);Num(3)])]);Num(0);Con([Num(2)]);Num(4)])]);
Con([Con([Con([Con([Num(8)]);Num(7);Num(8);Num(5);Con([Num(6)])]);Con([Num(3)]);Con([Con([Num(0);Num(6)]);Con([Num(10)]);Con([Num(7);Num(10);Num(6);Num(3);Num(9)])])]);Con([Con([]);Con([]);Con([Num(0);Con([Num(9);Num(5);Num(3);Num(3)]);Con([Num(4)])]);Con([Num(9);Con([Num(8);Num(5);Num(8);Num(4);Num(4)]);Con([Num(4)]);Con([Num(9);Num(7);Num(9)])])]);Con([Con([Con([Num(0);Num(2)]);Con([Num(0);Num(10);Num(9)]);Num(9)]);Con([Num(3)]);Con([])]);Con([Con([Num(9)]);Con([Num(3);Num(7)]);Num(10)]);Con([])]);
Con([Con([Con([Con([]);Con([])]);Num(5);Num(4)]);Con([Num(5);Con([Con([Num(10);Num(1);Num(6);Num(3);Num(7)]);Con([Num(10);Num(6);Num(10);Num(9);Num(3)]);Num(2);Con([Num(7);Num(1);Num(5);Num(3);Num(8)]);Num(1)]);Con([Num(1);Con([Num(4);Num(1)]);Con([]);Con([])]);Con([Num(6)]);Con([Num(9);Num(7);Num(2)])])]);
Con([Con([Con([Num(10);Num(8);Num(8);Con([])])])]);
Con([Con([Con([Num(5);Con([Num(7);Num(10)]);Con([Num(1)]);Num(5);Con([Num(0);Num(5)])]);Con([Num(8)]);Con([Con([Num(0);Num(5);Num(3);Num(4)])]);Num(4)])]);
Con([Con([Con([Num(5);Con([]);Num(0)]);Con([Con([Num(7);Num(8)]);Con([Num(6)])]);Con([Num(4);Con([])]);Num(7)]);Con([Con([Con([]);Con([Num(6);Num(10);Num(5);Num(9);Num(0)]);Num(1);Con([Num(2);Num(9)])]);Con([]);Con([]);Con([]);Num(9)]);Con([Num(3);Num(8);Con([Con([Num(3);Num(3);Num(8);Num(2);Num(7)]);Num(0)]);Num(2)]);Con([Con([Num(7);Con([Num(4)])]);Con([Num(4);Num(2)]);Con([Num(3);Num(1)]);Con([])])]);
Con([Con([Con([Con([Num(6)]);Con([]);Num(2);Con([]);Con([Num(6);Num(0)])]);Num(10);Con([Num(10)])]);Con([Con([Con([Num(5);Num(5);Num(10);Num(0);Num(3)])])]);Con([Num(6);Con([Num(6);Con([Num(7);Num(10);Num(5);Num(0);Num(2)]);Con([Num(2);Num(8);Num(5);Num(3);Num(6)]);Con([Num(5);Num(8)]);Num(10)]);Num(1);Num(10);Num(6)]);Con([Num(9);Con([Num(7);Num(5)]);Num(2);Con([Num(6);Con([Num(9);Num(7);Num(0);Num(2)]);Num(3);Num(2);Num(7)])])]);
Con([Con([]);Con([]);Con([]);Con([])]);
Con([Con([Num(1);Con([Num(1);Num(10);Con([Num(10);Num(3);Num(2);Num(6)]);Num(5)])]);Con([Num(9);Num(6);Num(10)])]);
Con([Con([Con([Con([Num(3);Num(7);Num(10);Num(3)]);Num(6);Con([Num(1);Num(6);Num(6);Num(9)]);Num(3);Con([Num(2);Num(9);Num(7);Num(10)])]);Con([Con([Num(8);Num(4);Num(5);Num(8);Num(6)]);Num(3);Num(5)]);Con([Con([Num(6);Num(3)])])]);Con([Con([Con([]);Con([Num(5)]);Con([Num(9);Num(10);Num(0);Num(6)]);Con([Num(7)])]);Con([]);Num(7);Num(10);Num(5)])]);
Con([Con([Con([])]);Con([Num(0);Con([])]);Con([Num(1);Con([]);Num(4);Con([Num(9);Con([Num(10)]);Num(0);Num(5)])]);Con([]);Con([])]);
Con([Con([]);Con([]);Con([Num(5);Num(5);Con([Num(5);Con([Num(4);Num(7);Num(6);Num(3);Num(4)]);Con([Num(10);Num(8)]);Num(0)])]);Con([Num(9);Con([]);Con([Num(10);Con([])])])]);
Con([Con([Con([Con([Num(10);Num(4);Num(0);Num(5)])])])]);
Con([Con([]);Con([Con([Con([Num(3);Num(1);Num(6);Num(9)]);Num(5);Con([Num(5)]);Num(8);Num(0)]);Con([Con([Num(8);Num(5);Num(4)])]);Num(4);Con([Con([Num(9);Num(0);Num(3)]);Con([Num(5);Num(9);Num(3);Num(10)]);Con([Num(8);Num(10);Num(5);Num(8)]);Con([Num(7);Num(4);Num(2);Num(1);Num(4)]);Num(2)]);Num(9)])]);
Con([Con([Num(6)]);Con([Num(0);Con([Con([Num(2);Num(5);Num(8);Num(7)]);Con([Num(1);Num(3);Num(5);Num(4)]);Con([Num(7)])]);Num(0);Con([Num(7)])]);Con([Num(6);Con([Num(4);Num(2)])]);Con([Con([Num(2)])])]);
Con([Con([Num(3);Con([Num(9);Con([Num(10);Num(5)])]);Con([Con([Num(9);Num(9);Num(5);Num(2)]);Con([Num(4);Num(3);Num(7)]);Con([Num(6);Num(9);Num(4);Num(6)]);Con([Num(3);Num(4);Num(9);Num(3);Num(10)])]);Num(5)]);Con([Con([Num(0);Con([Num(2);Num(0);Num(6);Num(3)]);Con([Num(6);Num(4);Num(2)]);Con([Num(4);Num(8);Num(0);Num(1);Num(1)]);Con([Num(6);Num(6);Num(7)])]);Num(0);Num(0);Num(3)]);Con([Con([Con([Num(10);Num(3);Num(2);Num(10);Num(7)]);Con([Num(8);Num(6)])]);Con([Num(1)]);Con([Con([Num(3);Num(4);Num(6);Num(7)]);Con([Num(6);Num(2);Num(1)]);Con([Num(7)]);Num(2)]);Num(5)]);Con([Num(2);Con([Con([Num(0);Num(6)]);Num(5);Num(8);Num(2);Num(9)]);Con([Num(10);Con([Num(6);Num(2);Num(9)]);Num(2)])])]);
Con([Num(6);Num(1);Num(4);Num(0);Num(3)]);
Con([Num(6);Num(1);Num(4);Num(0)]);
Con([Con([Con([Con([Num(5)]);Num(6)]);Con([Num(4);Num(2);Num(10);Con([Num(1);Num(8);Num(8)]);Num(7)]);Con([Con([]);Con([Num(4)]);Con([Num(7);Num(4);Num(4);Num(4)]);Con([Num(2)])])]);Con([Con([Con([Num(0);Num(5)]);Num(9);Con([Num(5)])])]);Con([Num(6);Con([Num(2);Num(6);Con([Num(1);Num(1);Num(6);Num(10);Num(3)]);Num(7)]);Con([Num(1);Con([]);Con([Num(8)]);Con([Num(0);Num(10);Num(9);Num(3);Num(0)]);Num(8)])]);Con([Num(1);Con([Num(1);Num(8)]);Con([Num(4)]);Num(7);Con([])])]);
Con([Con([Con([Con([Num(7);Num(9);Num(6);Num(7);Num(6)])]);Con([])])]);
Con([Con([Con([Num(6)])]);Con([Con([Con([Num(4);Num(4)]);Num(3);Con([Num(9);Num(1);Num(2)])]);Con([Con([Num(3);Num(2)])]);Num(0);Con([Num(4)])]);Con([Num(0);Num(8);Num(8)])]);
Con([Con([Num(9);Num(3);Con([Num(4)]);Num(4);Con([Con([Num(4)]);Con([]);Con([Num(1);Num(1);Num(10)]);Num(10);Con([Num(9);Num(7);Num(5);Num(3);Num(6)])])]);Con([Num(6);Con([Num(2);Num(6)])]);Con([Con([])]);Con([Con([Num(7);Con([Num(2);Num(7)]);Con([Num(3);Num(7);Num(9)]);Num(0)])]);Con([])]);
Con([Con([Num(2);Con([]);Con([]);Num(6);Con([Num(7);Con([Num(4);Num(4)]);Con([Num(7);Num(7);Num(3);Num(4);Num(7)])])]);Con([Con([Con([Num(1);Num(1);Num(3);Num(8);Num(5)]);Num(9);Con([Num(10)]);Num(3)]);Con([Num(7);Num(7)]);Num(0);Num(7);Num(7)]);Con([Con([]);Num(9)]);Con([Num(5);Con([]);Num(2);Con([Con([]);Num(7);Con([Num(1)]);Con([Num(6);Num(4)])]);Num(10)]);Con([Con([Num(1);Num(8);Con([Num(2);Num(10)]);Num(2);Con([Num(4);Num(10)])]);Con([Con([Num(10);Num(7);Num(9);Num(0)]);Con([]);Num(8);Num(1);Con([Num(7);Num(0);Num(3);Num(5)])]);Con([Con([Num(9);Num(3)]);Num(0);Num(4);Num(4);Con([Num(9);Num(6);Num(1)])]);Num(6)])]);
Con([Con([]);Con([Con([Num(2)]);Con([Con([Num(10)]);Num(10);Con([Num(7);Num(4);Num(0)]);Num(10);Num(5)]);Num(3);Num(2);Num(9)]);Con([Num(9)]);Con([Con([Con([Num(4);Num(10)])]);Num(4);Num(4);Con([Con([Num(2);Num(7);Num(7)]);Num(8)]);Con([Num(9);Num(6);Num(6);Num(0);Num(4)])])]);
Con([Con([Num(2);Con([])]);Con([]);Con([Num(9);Con([Num(7);Num(5)]);Num(5);Num(10)]);Con([Con([Con([Num(2);Num(6)]);Con([Num(3);Num(1);Num(9)]);Num(4);Num(6)]);Con([Con([Num(3);Num(8);Num(3)]);Con([Num(4);Num(9);Num(6);Num(3);Num(0)]);Num(5);Con([Num(9);Num(3);Num(3)]);Con([Num(10);Num(6);Num(2)])]);Con([])]);Con([Num(7);Num(7);Con([]);Num(2)])]);
Con([Con([Con([Con([Num(6);Num(1);Num(3);Num(10)]);Num(2);Con([Num(6);Num(0)]);Con([]);Num(0)])]);Con([Num(9);Num(9);Num(0);Con([Con([Num(0);Num(2);Num(9);Num(10);Num(1)])])])]);
Con([Con([Con([Num(5);Con([Num(0);Num(0);Num(1)]);Con([Num(2);Num(3);Num(7);Num(0)])]);Con([Con([Num(2);Num(9);Num(10);Num(10);Num(6)]);Con([Num(10);Num(2);Num(1);Num(7)]);Con([Num(3)]);Con([]);Num(1)]);Con([]);Con([Num(0)]);Num(7)]);Con([Num(7);Num(5);Num(4);Con([Num(1);Num(5);Num(4)])]);Con([Num(8);Num(6);Con([Num(2)]);Con([])]);Con([Con([Con([]);Con([Num(10);Num(7);Num(5)]);Con([Num(6)])])]);Con([Num(6)])]);
Con([Con([Num(3)]);Con([]);Con([Num(5);Con([Num(9);Con([Num(3);Num(10);Num(7)]);Num(6)]);Con([Num(1);Con([Num(5);Num(6);Num(5);Num(2);Num(5)])]);Con([]);Con([Con([Num(7);Num(9)]);Num(7)])])]);
Con([Con([Num(9);Con([Con([Num(5);Num(7)])]);Con([Num(8);Con([Num(5)]);Num(7)])]);Con([Num(4);Con([Con([Num(2)]);Con([Num(0);Num(3);Num(1);Num(8);Num(9)]);Con([Num(7);Num(2);Num(4)])]);Num(8)])]);
Con([Con([]);Con([Con([Con([Num(8);Num(6);Num(0);Num(7)]);Con([Num(1);Num(9);Num(1)])]);Con([Num(9);Con([Num(9);Num(9)])]);Num(1);Con([Con([Num(7);Num(8);Num(6)])]);Num(9)])]);
Con([Con([Con([Num(9);Con([Num(10);Num(5);Num(4)])]);Num(5);Con([Con([Num(7);Num(6)]);Con([Num(8)])])]);Con([Num(4)]);Con([]);Con([Con([]);Num(0)]);Con([Con([]);Con([Con([]);Con([Num(1);Num(10);Num(10);Num(5);Num(0)]);Num(8);Con([Num(5)])]);Con([Con([Num(1);Num(8);Num(10);Num(9);Num(8)]);Con([Num(4)]);Num(7);Num(1)]);Num(0)])]);
Con([Con([Num(0);Num(0);Con([Num(6);Con([Num(0)])]);Num(5)]);Con([Num(3);Num(1);Num(3);Con([Num(5)])])]);
Con([Con([Num(0)]);Con([Num(0);Num(9);Num(9);Con([Num(9);Num(7)])])]);
Con([Con([Con([Con([Num(3);Num(1);Num(5);Num(9)])]);Con([Num(3);Con([Num(3);Num(4);Num(9)]);Con([Num(4);Num(2)]);Num(0)]);Con([Num(8);Con([]);Con([])]);Num(4);Num(1)]);Con([Num(4);Num(3);Num(10);Num(9)]);Con([Num(3);Con([Con([Num(4);Num(3);Num(7);Num(0);Num(4)]);Con([Num(1);Num(6);Num(5);Num(8);Num(6)]);Con([Num(10)]);Num(4)]);Num(1);Num(3);Con([Num(0);Con([Num(2);Num(4);Num(0);Num(0);Num(8)])])]);Con([Num(6);Con([]);Con([Con([Num(4);Num(7);Num(8)]);Num(9);Num(2);Num(1);Num(2)]);Con([]);Num(3)]);Con([Num(2);Con([Con([Num(9);Num(4)]);Num(7);Num(8);Con([Num(10)]);Num(5)]);Con([]);Num(4)])]);
Con([Con([Con([Con([Num(2);Num(6);Num(10);Num(5);Num(0)]);Num(9)]);Con([]);Con([Num(10);Con([Num(5);Num(4);Num(9)]);Num(0)]);Con([Con([Num(6);Num(4)])]);Con([Con([Num(5);Num(7);Num(6)])])])]);
Con([Con([Num(10)]);Con([Con([Num(4);Num(8);Con([Num(9);Num(9);Num(10);Num(5)]);Num(7);Num(5)]);Num(4)]);Con([Num(3);Con([Con([]);Num(6);Con([Num(10);Num(9);Num(9);Num(1);Num(9)])])]);Con([Num(10)]);Con([Num(4)])]);
Con([Con([Con([Con([Num(4);Num(4);Num(3);Num(9);Num(6)]);Con([Num(2)])]);Con([Num(5);Num(2);Con([Num(4)])]);Num(0);Con([Num(6);Con([Num(3);Num(9);Num(2)]);Num(3);Con([Num(8);Num(1);Num(7)]);Num(3)])])]);
Con([Con([Con([Con([Num(1);Num(9);Num(9);Num(3)])]);Con([]);Num(0);Con([]);Num(0)]);Con([Num(10);Con([Num(5);Num(3);Con([])]);Num(0)])]);
Con([Con([Num(5);Num(3);Num(2);Con([]);Num(2)]);Con([Num(4);Num(6);Num(4)])]);
Con([Con([Con([Con([Num(3);Num(0)])]);Num(6)]);Con([Num(4);Con([Num(10)]);Num(7);Con([Num(9)])]);Con([Num(7);Con([Con([]);Con([Num(5);Num(5)]);Num(0)])]);Con([])]);
Con([Con([Num(5);Num(1);Num(9);Num(8);Num(7)]);Con([]);Con([Num(5);Con([Con([Num(0);Num(6);Num(9);Num(6);Num(1)]);Con([Num(9);Num(10);Num(3);Num(9);Num(0)]);Con([Num(10);Num(7);Num(8)])]);Num(1);Num(7);Con([Con([Num(0);Num(0);Num(1);Num(2)]);Con([Num(4)]);Con([Num(0);Num(4);Num(5);Num(6)]);Num(5)])])]);
Con([Con([]);Con([Num(0);Num(8);Num(5)])]);
Con([Con([Con([Num(3);Con([Num(8);Num(9);Num(5)]);Con([Num(8);Num(2);Num(10);Num(4)]);Con([Num(7);Num(7);Num(6);Num(2)]);Num(6)])]);Con([Num(9);Num(9);Con([]);Num(1)]);Con([Con([])]);Con([Con([Num(9);Con([Num(9);Num(1);Num(5)]);Con([Num(0);Num(10);Num(7);Num(9)]);Con([Num(0);Num(5);Num(9);Num(8)])]);Num(10);Num(8);Con([Con([Num(6)]);Con([Num(3);Num(10);Num(2)]);Num(3)])])]);
Con([Con([Num(10);Num(1);Num(1)])]);
Con([Con([Con([])])]);
Con([Con([Num(2);Con([Con([Num(0)]);Num(6);Con([Num(3)])]);Num(10);Con([Num(4);Con([Num(2);Num(1);Num(2);Num(10)])])])]);
Con([Con([Num(8);Num(8);Con([Num(3);Con([]);Con([Num(0)]);Num(7)]);Num(7);Con([Num(0);Con([Num(4);Num(4)])])]);Con([]);Con([Con([Num(2);Num(4);Con([Num(9);Num(3);Num(4);Num(1);Num(7)]);Num(8)]);Con([Num(3)]);Con([Num(9)]);Con([Con([Num(6);Num(5);Num(1);Num(10);Num(3)]);Con([Num(4);Num(8);Num(8)])])]);Con([Num(8);Con([Con([]);Num(2);Num(10);Con([Num(2);Num(4);Num(5);Num(6)]);Con([Num(4)])]);Num(4);Con([Con([Num(1);Num(4);Num(3);Num(2);Num(2)])])])]);
Con([Con([Num(4);Num(6)]);Con([Con([Con([Num(3);Num(4)]);Num(10);Num(10)])]);Con([])]);
Con([Con([Num(3)]);Con([Con([Num(2);Con([Num(9);Num(10);Num(6);Num(9)])])]);Con([Num(8)]);Con([Num(1);Con([Con([])]);Con([]);Con([Con([])]);Num(2)]);Con([Num(8);Num(6)])]);
Con([Con([Num(3);Con([]);Num(0);Con([Num(1);Con([Num(3);Num(8);Num(10);Num(4);Num(5)]);Num(1);Num(2)]);Num(0)]);Con([]);Con([Num(5);Num(7);Num(4);Num(2)]);Con([Con([Con([Num(9);Num(1);Num(3);Num(6);Num(6)])]);Num(6)])]);
Con([Con([Con([Num(4);Num(2);Num(5);Con([Num(9);Num(8)])])]);Con([]);Con([Num(3);Con([Num(6);Con([Num(0);Num(9);Num(5);Num(0);Num(5)]);Con([Num(4);Num(10);Num(3);Num(7)]);Num(1);Num(2)]);Con([Num(10)]);Num(2)])]);
Con([Con([Num(3);Num(6);Con([]);Num(4)]);Con([Con([Con([Num(10);Num(6)])])]);Con([]);Con([Num(8);Con([Con([Num(2);Num(3);Num(2)]);Con([Num(1);Num(8)]);Num(1)])]);Con([])]);
Con([Con([Num(8);Con([Num(4)]);Con([Num(10);Con([])]);Num(5)]);Con([])]);
Con([Con([Con([Num(5);Con([Num(3);Num(0);Num(0)])]);Num(9);Con([]);Num(6);Num(2)])]);
Con([Con([Num(3);Con([Num(2);Num(3);Con([Num(0)])])]);Con([Con([]);Con([]);Num(0);Num(5);Num(8)])]);
Con([Con([Num(10)]);Con([Con([]);Num(0);Con([Con([Num(8);Num(1);Num(8)]);Num(5);Num(0);Num(0)]);Num(9)]);Con([Num(4);Con([Num(1);Num(3);Num(3);Num(0)]);Con([])]);Con([Num(7);Num(2);Num(2)]);Con([])]);
Con([Con([Num(9);Con([Con([Num(3);Num(8);Num(9)])])])]);
Con([Con([]);Con([Con([]);Num(2);Con([Con([Num(4);Num(6);Num(10);Num(3);Num(4)])])]);Con([Con([Num(9);Num(4);Con([Num(3)]);Con([Num(1);Num(3);Num(7)]);Con([Num(3);Num(8);Num(4)])]);Num(8)]);Con([Num(4);Num(9);Num(6)]);Con([Con([Num(5);Num(10)]);Con([Num(3);Num(1);Con([Num(9);Num(2);Num(8);Num(8);Num(1)])])])]);
Con([Con([Num(9);Num(10);Num(6);Num(6)]);Con([Con([Con([Num(9);Num(1);Num(2);Num(7);Num(7)])]);Con([]);Num(8);Con([])]);Con([Con([]);Num(5)]);Con([Num(4);Con([Con([Num(3);Num(1);Num(6);Num(7);Num(0)])]);Con([Num(7);Con([Num(9);Num(7);Num(8)]);Con([Num(9);Num(5);Num(2)])])]);Con([Con([]);Con([Con([Num(0);Num(1);Num(1)])]);Num(8);Con([])])]);
Con([Con([])]);
Con([Con([Num(9);Num(7);Con([Num(4);Num(2);Num(1);Num(8)]);Con([Num(3);Num(5);Con([Num(9);Num(2);Num(2)])])]);Con([Con([Num(5);Con([Num(3);Num(9);Num(5);Num(3)])]);Num(4);Con([Con([]);Con([Num(5)]);Con([Num(5);Num(3);Num(1)])]);Num(8)])]);
Con([Con([]);Con([]);Con([Num(2);Num(9);Con([]);Num(0)]);Con([Con([Con([Num(3);Num(10);Num(5)]);Num(7);Con([Num(7)])])])]);
Con([Con([Num(7);Con([Con([Num(4);Num(0);Num(6)]);Num(7)]);Con([Num(8)]);Num(10);Con([Con([Num(3);Num(0)]);Num(0);Con([Num(4);Num(4);Num(9)])])]);Con([]);Con([Con([Num(10)])]);Con([Num(4)]);Con([Num(10);Num(3);Num(1);Num(7)])]);
Con([Con([Num(5)])]);
Con([Con([Num(6);Num(10);Num(1)]);Con([Con([Con([Num(6);Num(3);Num(4);Num(7)]);Con([Num(0);Num(9);Num(5);Num(7);Num(5)]);Con([]);Num(6);Num(4)]);Con([Con([Num(8);Num(2);Num(2);Num(3)])]);Num(0);Con([Num(5);Num(9);Con([Num(10);Num(6)]);Num(2);Con([Num(0);Num(5);Num(0)])])]);Con([])]);
Con([Con([Con([Num(5);Num(6)]);Num(6);Con([Con([Num(4)]);Num(4);Num(3);Con([Num(7);Num(1)]);Con([Num(10);Num(5);Num(4)])])])]);
Con([Con([Num(2)]);Con([Num(9);Con([]);Num(5);Num(10)]);Con([]);Con([]);Con([])]);
Con([Con([Num(10)]);Con([Con([Con([Num(4);Num(5);Num(2);Num(6);Num(10)]);Num(5);Con([Num(0)])]);Num(4);Con([Num(2);Num(9);Con([Num(4)]);Num(8)]);Num(1);Con([Con([Num(5);Num(6);Num(10)]);Con([Num(9);Num(6)]);Num(6);Con([Num(2);Num(8)])])])]);
Con([Con([Con([Con([Num(9);Num(5);Num(3)]);Con([Num(0);Num(5);Num(3)]);Con([Num(8);Num(7);Num(0);Num(2);Num(6)])]);Num(4)]);Con([Con([Con([Num(6);Num(4);Num(2);Num(10)]);Con([Num(6);Num(2)]);Num(8)]);Con([Num(10);Con([Num(7)]);Num(6);Con([Num(10);Num(3);Num(4);Num(5);Num(3)])]);Num(3);Num(4);Num(8)]);Con([Num(6);Num(3);Num(0)])]);
Con([Con([Num(2);Num(0)])]);
Con([Con([Con([Num(8);Con([Num(8);Num(7)]);Num(6);Num(0);Num(2)]);Num(2);Con([Con([Num(5);Num(10)]);Num(2);Num(4);Num(1);Con([Num(7);Num(1);Num(5)])]);Con([Con([Num(8);Num(8);Num(5)]);Num(7)])])]);
Con([Con([Con([]);Con([Con([]);Con([]);Num(10);Num(8);Con([Num(10);Num(10)])]);Num(9);Con([])]);Con([Con([Num(7);Con([Num(6);Num(1);Num(9);Num(7);Num(6)]);Con([Num(10);Num(6);Num(6);Num(4);Num(10)]);Num(9);Con([Num(2);Num(3)])]);Num(0);Con([Con([Num(1)])]);Con([])]);Con([]);Con([Num(4);Con([Con([Num(9);Num(5);Num(10)])]);Num(10);Num(5)]);Con([Con([Num(5);Num(9);Num(9)])])]);
Con([Con([Num(7);Con([]);Num(6);Con([]);Con([Con([Num(4)]);Con([Num(0);Num(1);Num(3);Num(0);Num(9)]);Con([Num(7);Num(10);Num(0);Num(9)])])]);Con([Num(3);Con([Con([Num(3);Num(10);Num(6)]);Num(4);Con([Num(10);Num(3);Num(8)])]);Num(2);Con([])])]);
Con([Con([Con([Num(1);Con([Num(2);Num(5);Num(9);Num(9)]);Con([Num(6);Num(1);Num(10);Num(8);Num(8)]);Num(5);Con([])]);Num(10);Con([Num(2)]);Num(1);Num(0)]);Con([Con([Con([Num(1);Num(0);Num(5)])]);Con([Num(8);Num(9);Con([])]);Con([Con([Num(3);Num(0);Num(6);Num(4)]);Num(8);Con([Num(7);Num(1)])]);Con([Num(1);Con([Num(4);Num(8);Num(6);Num(7)]);Con([])]);Num(4)]);Con([Num(2)]);Con([Con([Con([Num(6);Num(2);Num(5);Num(3);Num(2)])]);Num(1)])]);
Con([Con([Num(1);Num(0);Num(7);Con([Num(3);Num(2);Con([Num(10);Num(4)]);Num(5)]);Con([])]);Con([Num(8)]);Con([Num(7)]);Con([Con([Con([Num(0);Num(4)]);Con([]);Con([Num(5)]);Num(0);Con([])]);Con([]);Con([Num(7)]);Con([])])]);
Con([Con([Con([]);Num(10);Con([Con([Num(2);Num(8);Num(3);Num(1);Num(8)]);Con([]);Num(7);Con([])]);Num(3);Num(9)]);Con([Con([Con([Num(1);Num(2);Num(7)])]);Num(2)]);Con([Con([Num(4)])]);Con([Num(0);Num(7);Num(8)]);Con([Num(5);Con([Num(6);Num(4)]);Num(5);Num(1);Num(7)])]);
Con([Con([]);Con([Con([Num(7);Con([Num(1);Num(7);Num(6)]);Con([Num(4);Num(10);Num(5);Num(4)]);Con([]);Num(9)]);Con([Con([]);Num(7);Con([]);Num(4);Num(7)])]);Con([Num(8);Num(10);Num(6);Num(9);Con([Num(9);Con([Num(3);Num(10)]);Con([Num(5);Num(10);Num(10);Num(2)]);Con([Num(6);Num(4);Num(3)]);Con([])])]);Con([Con([Num(5);Num(2)]);Num(7);Num(9);Num(1)]);Con([Con([Num(10);Num(9);Con([Num(4);Num(6);Num(6);Num(9)])]);Num(10);Num(1);Num(8);Num(1)])]);
Con([Con([Num(10);Num(2);Con([Num(8)]);Con([Con([Num(5);Num(9);Num(0)]);Con([Num(6);Num(7);Num(4);Num(8);Num(8)]);Con([])]);Con([])]);Con([Con([Num(10);Con([Num(3);Num(1);Num(7)]);Con([Num(3);Num(1);Num(9);Num(8);Num(0)]);Num(2);Num(7)]);Con([Num(5);Con([Num(6);Num(1);Num(9);Num(10);Num(3)]);Num(10)])]);Con([Con([Num(0);Con([Num(7);Num(2)]);Num(2);Con([Num(4);Num(6);Num(9);Num(1);Num(9)])]);Num(1)]);Con([])]);
Con([Con([Con([Num(9);Con([Num(6);Num(8)])]);Num(3);Con([Con([Num(5);Num(9);Num(7)]);Num(2);Num(7);Num(10)]);Con([Num(3);Num(3);Con([Num(5);Num(10);Num(0);Num(7)]);Con([Num(6);Num(9);Num(8)]);Num(9)]);Num(4)]);Con([])]);
Con([Con([Num(3);Num(2)]);Con([Num(5);Con([Con([Num(7);Num(0);Num(3)]);Num(3);Con([Num(5);Num(6)]);Con([Num(1);Num(7);Num(10)]);Num(8)]);Num(9)]);Con([Con([Con([Num(5)]);Num(0)]);Num(1);Con([Con([Num(3);Num(8);Num(0);Num(0);Num(4)]);Con([Num(0)])]);Con([Num(4);Num(8)]);Con([Num(10);Con([Num(3);Num(9);Num(5);Num(5)]);Con([Num(9);Num(4)]);Num(8)])])]);
Con([Con([Num(0);Num(0)]);Con([Con([Num(1);Con([Num(1);Num(7);Num(7);Num(9)])]);Con([Num(2);Num(0)]);Num(3);Num(5)]);Con([Num(1);Con([Con([]);Con([Num(1);Num(9);Num(6);Num(7);Num(10)]);Con([Num(4);Num(7)])]);Num(1);Num(9)]);Con([Con([Con([Num(8)]);Con([Num(1);Num(0);Num(3);Num(2);Num(9)]);Num(4);Num(5);Num(3)]);Con([Con([Num(1);Num(1);Num(3)]);Num(5);Num(10);Con([])]);Con([Num(5);Con([Num(7);Num(8)])])]);Con([Con([Num(5);Num(9);Num(5)]);Con([Con([Num(1);Num(0);Num(9)]);Num(6)]);Num(4);Num(1);Num(8)])]);
Con([Con([Con([Num(1);Con([]);Num(10);Num(10);Num(4)])]);Con([]);Con([]);Con([]);Con([Num(0);Num(4);Con([Num(1);Con([Num(3)]);Con([Num(1)]);Con([]);Con([Num(0);Num(7);Num(7);Num(1);Num(8)])]);Con([Con([Num(3)]);Num(5);Con([Num(3)]);Num(2)])])]);
Con([Con([Num(10);Num(9)]);Con([Num(2);Num(10);Num(9);Num(6);Num(8)]);Con([Num(3);Con([Con([Num(8);Num(10);Num(6);Num(10);Num(6)]);Con([Num(7)]);Con([Num(4);Num(8);Num(10);Num(10);Num(0)]);Con([])]);Num(9);Num(4);Num(1)]);Con([Num(3);Num(4);Con([]);Con([]);Con([])]);Con([Num(8);Num(7)])]);
Con([Con([Con([Con([Num(2);Num(3);Num(0);Num(1);Num(1)]);Num(3);Num(9);Con([Num(0)]);Num(8)]);Con([Num(5);Num(5);Num(1);Con([])]);Num(0);Con([])]);Con([Num(8);Con([Con([]);Con([Num(8)]);Num(5);Num(2);Num(8)]);Con([Num(1);Con([Num(7);Num(2)]);Con([Num(9);Num(2);Num(8)]);Num(4)])]);Con([Con([Con([Num(10);Num(6);Num(10);Num(1);Num(9)])])]);Con([Num(7);Con([Num(10);Num(10);Con([])]);Num(2);Con([Con([Num(5)]);Num(1);Num(9);Con([Num(4);Num(3);Num(2)]);Con([Num(3);Num(5);Num(0);Num(9)])]);Con([Con([Num(0)]);Num(0);Con([Num(7)])])]);Con([Con([Num(1);Con([Num(7);Num(0)]);Con([Num(5);Num(6);Num(7)])]);Num(4);Num(0);Con([])])]);
Con([Con([Con([]);Con([])]);Con([Num(7)]);Con([Num(0);Con([Con([Num(6);Num(3);Num(9);Num(9);Num(6)])])]);Con([Num(7);Con([Num(10);Num(2);Num(10)]);Con([]);Num(5)])]);
Con([Con([Con([Con([Num(10);Num(6);Num(0);Num(10)]);Num(9);Num(1)]);Num(6);Num(10)]);Con([Con([Con([Num(0);Num(7);Num(0);Num(9)]);Con([Num(5);Num(0)]);Num(8);Con([Num(9)]);Num(0)]);Num(7);Num(8);Con([Num(6);Num(6);Con([Num(4);Num(5);Num(9)])])]);Con([])]);
Con([Con([Num(4);Num(6);Num(0);Con([Num(9);Num(8);Num(6);Con([Num(6);Num(9);Num(5);Num(0);Num(3)]);Num(4)]);Num(2)]);Con([Num(1)]);Con([Num(3)]);Con([Con([Con([Num(0);Num(1);Num(3);Num(10);Num(6)]);Con([Num(8);Num(3);Num(7);Num(4)]);Con([Num(10);Num(10);Num(0)])])]);Con([])]);
Con([Con([Num(8);Num(6);Num(10);Num(1);Num(2)]);Con([Con([Num(5)])])]);
Con([Con([Con([Num(5);Con([Num(1);Num(3)]);Con([Num(7);Num(7);Num(2);Num(1)])]);Num(1);Num(3)])]);
Con([Con([Con([Num(3);Num(8);Num(5);Num(10)]);Con([Num(9)]);Num(10)]);Con([Num(9);Con([])]);Con([Con([Num(10);Con([]);Num(1);Con([]);Num(10)]);Con([Con([Num(9);Num(5);Num(7)])]);Con([Con([Num(8);Num(5);Num(6)]);Con([Num(4)])]);Num(5);Con([Con([])])]);Con([]);Con([Num(0);Con([Num(7)])])]);
Con([Con([]);Con([Con([Con([Num(7)]);Num(2);Num(4)]);Con([Con([Num(4);Num(5);Num(0);Num(5)]);Con([]);Num(10);Num(6)]);Num(8)]);Con([Con([Num(0);Num(7);Num(3);Num(0)]);Con([]);Con([]);Con([Num(2)]);Num(3)]);Con([Num(5)])]);
Con([Con([Num(7)]);Con([Con([Con([Num(7);Num(3);Num(5);Num(7);Num(5)]);Num(7);Con([Num(7);Num(1);Num(9);Num(2)])]);Num(10);Con([Num(7)]);Num(4);Con([Con([]);Num(10);Num(10);Con([Num(10);Num(5);Num(5)])])]);Con([]);Con([Con([]);Con([Num(9)])]);Con([Num(6)])]);
Con([Con([Num(10);Con([Con([Num(1);Num(10);Num(3)]);Con([Num(2)]);Num(6);Con([Num(6);Num(3);Num(5);Num(8)]);Con([Num(5);Num(1);Num(7);Num(8);Num(10)])]);Con([Con([Num(3);Num(3);Num(5);Num(6);Num(0)]);Num(8);Num(10)]);Con([Con([Num(3);Num(1);Num(7)]);Num(6);Con([Num(2);Num(9);Num(9)]);Num(7);Con([Num(5);Num(5)])]);Con([Con([Num(8)]);Con([Num(0);Num(2);Num(1);Num(0);Num(0)])])]);Con([]);Con([Num(3);Con([Con([]);Num(0);Num(4);Num(7)]);Num(0);Con([Num(0)])]);Con([Con([Num(0)]);Num(9);Num(2);Con([Con([Num(10)])])])]);
Con([Con([Num(7);Num(8);Con([]);Con([Con([Num(8);Num(6);Num(3);Num(7);Num(10)]);Num(7);Con([Num(4);Num(8);Num(6)]);Con([Num(8);Num(4);Num(4);Num(2);Num(3)]);Num(5)]);Con([Num(3);Con([Num(9);Num(3);Num(2);Num(0);Num(9)])])])]);
Con([Con([Con([]);Con([Con([Num(4);Num(7)]);Num(3);Con([Num(0);Num(6);Num(4);Num(1)]);Num(9);Num(5)]);Con([Con([Num(1)]);Con([Num(2);Num(8)])]);Num(0)]);Con([Num(10);Num(1)]);Con([]);Con([Con([Con([Num(2);Num(5)]);Con([Num(10);Num(0);Num(0);Num(4);Num(6)])]);Num(9);Con([Con([Num(7);Num(4)]);Con([Num(0);Num(7)]);Num(7)]);Num(3);Con([Con([Num(3);Num(9);Num(1)]);Num(6)])])]);
Con([Con([Con([Con([Num(10);Num(5);Num(6)]);Num(5);Con([Num(8);Num(4);Num(5)]);Con([Num(8);Num(10)])]);Num(3);Con([Con([Num(10);Num(7)]);Con([Num(9);Num(1);Num(7);Num(2)]);Num(9);Num(8);Num(8)]);Con([Num(1)])]);Con([Num(8);Con([]);Num(10);Con([]);Num(6)])]);
Con([Con([Num(6)]);Con([]);Con([Con([Con([Num(0);Num(8);Num(2)]);Num(7)]);Con([Con([Num(3)]);Num(10);Num(0)])]);Con([Num(0);Con([Con([Num(8);Num(0);Num(4);Num(10)]);Num(2)]);Num(3);Con([Con([Num(1);Num(10);Num(7)]);Num(10);Num(0);Con([Num(1);Num(7);Num(2);Num(7)]);Num(1)])]);Con([Num(4);Num(0);Con([Num(8);Num(3);Num(4);Num(8);Con([])]);Num(10);Con([Num(1);Con([Num(8);Num(1);Num(2);Num(0)])])])]);
Con([Con([Con([Num(9);Con([Num(10);Num(3)]);Con([Num(7);Num(8)])]);Num(7);Con([Con([Num(6);Num(4);Num(3)]);Num(0);Con([Num(5);Num(0);Num(6);Num(0);Num(2)]);Con([Num(2);Num(0);Num(0)]);Con([Num(10);Num(3);Num(6);Num(2)])])]);Con([Num(10);Num(2);Num(5)]);Con([Con([Con([Num(3)]);Con([Num(5);Num(0);Num(10);Num(7)])]);Con([Con([Num(8)]);Num(6)])]);Con([Con([Con([Num(1)]);Con([Num(6);Num(9);Num(8);Num(10)]);Num(8)]);Num(1)])]);
Con([Con([Con([Con([Num(4)]);Num(10)])]);Con([Con([Num(9);Con([Num(4);Num(2);Num(6);Num(3);Num(6)]);Num(3)]);Num(4)]);Con([Num(0);Con([Con([])])]);Con([Num(6);Num(6);Con([Num(4);Num(2);Con([Num(5)]);Num(9)]);Con([Con([Num(5);Num(4);Num(5);Num(3)]);Num(0);Num(8);Num(4)]);Num(8)])]);
Con([Con([]);Con([Con([Num(0);Con([]);Con([Num(2);Num(7);Num(0);Num(4)])]);Num(3)]);Con([Num(8);Con([Con([Num(1);Num(4);Num(4)]);Con([Num(9);Num(7);Num(9);Num(3);Num(8)]);Con([Num(7);Num(1);Num(6);Num(1)])]);Num(2);Num(7)])]);
Con([Con([Num(9);Con([Num(0)]);Num(8);Num(7)]);Con([Num(7);Con([Num(7);Con([Num(0);Num(3)]);Num(2);Con([Num(8);Num(1);Num(6)]);Con([Num(5);Num(7);Num(7)])]);Num(5)]);Con([Con([Num(10);Num(1)])]);Con([])]);
Con([Con([Con([Num(0);Num(5);Num(8);Con([Num(4);Num(9);Num(6);Num(5)]);Num(6)])]);Con([Con([Con([Num(4);Num(4);Num(7);Num(5);Num(10)])]);Num(5);Con([Num(9);Num(10)]);Num(8);Num(5)]);Con([])]);
Con([Con([Num(7);Num(9);Con([Con([Num(10);Num(2);Num(6);Num(6)]);Num(5)]);Con([Con([Num(6);Num(2)]);Num(3);Con([Num(5);Num(7);Num(0);Num(7);Num(9)])]);Con([Con([Num(4);Num(4)]);Con([])])]);Con([Num(4);Con([Con([]);Num(7)]);Con([Num(10);Num(6);Num(3)]);Con([]);Num(6)]);Con([Num(3);Con([Num(9);Num(1);Num(4);Num(10);Con([Num(4);Num(9);Num(1)])]);Num(5)]);Con([Num(6)]);Con([Num(10);Num(2);Num(10)])]);
Con([Con([Con([Con([]);Con([Num(2);Num(10);Num(9)]);Con([Num(3);Num(6)]);Con([Num(10);Num(2)])]);Num(3);Num(8);Con([Num(10);Con([Num(7);Num(5)]);Num(3);Num(9)]);Con([Con([Num(9)]);Num(5);Num(6);Num(6);Num(8)])]);Con([Num(9);Num(6);Num(5);Num(7)]);Con([Num(2);Num(1);Num(1);Con([Con([Num(1)])])]);Con([Con([Con([Num(2)]);Con([Num(2)]);Num(6)])]);Con([])]);
Con([Con([]);Con([Num(6);Num(7);Con([Con([Num(6);Num(3);Num(7);Num(4);Num(7)]);Con([Num(6);Num(5);Num(6)]);Num(8)]);Con([Num(5);Con([Num(9)]);Con([Num(1);Num(0);Num(9);Num(7)]);Num(0)]);Num(7)])]);
Con([Con([Con([Num(4);Con([Num(5);Num(9);Num(9)])]);Con([Con([Num(4);Num(6);Num(2);Num(5);Num(10)]);Num(8);Con([Num(10);Num(8);Num(3)]);Num(2)]);Con([Num(9);Num(6);Num(1);Num(3);Num(0)])]);Con([Con([Num(4);Num(8);Num(6)]);Num(1);Num(5)])]);
Con([Con([Con([Num(6)]);Num(5);Num(7);Con([])]);Con([Num(8);Num(0);Con([]);Con([Num(1);Num(0);Num(0);Num(3);Num(3)])]);Con([Con([Num(6);Num(7)])]);Con([Con([Con([]);Num(3)]);Num(4)])]);
Con([Con([Con([Con([Num(2);Num(5);Num(3)])])]);Con([Con([]);Con([Con([Num(4);Num(3)]);Num(4);Con([Num(9);Num(3);Num(7)]);Con([Num(0);Num(7)])]);Con([Num(4)]);Con([Num(5);Con([Num(9);Num(9);Num(8);Num(7)]);Num(0);Num(1)])]);Con([Con([Num(7);Con([])]);Con([Con([Num(2);Num(1);Num(0);Num(3)])])])]);
Con([Con([]);Con([Con([Num(1)]);Con([Con([Num(5);Num(9);Num(6);Num(6);Num(6)]);Con([Num(6);Num(8);Num(2);Num(8);Num(2)]);Num(9);Con([Num(1);Num(8)]);Con([Num(3);Num(7)])]);Con([Num(6);Con([])])]);Con([Num(8);Num(1);Num(2);Num(5)])]);
Con([Con([Num(9)]);Con([Num(7);Con([Num(8);Con([Num(10);Num(5);Num(10);Num(3);Num(2)])]);Num(1);Con([]);Con([Num(2);Num(1);Con([Num(1);Num(8);Num(8);Num(3)]);Num(0)])]);Con([Con([]);Con([Con([Num(9);Num(8);Num(8);Num(1)]);Num(2);Num(7);Num(9)]);Num(7);Con([Con([Num(3);Num(4);Num(5);Num(3)]);Con([Num(6)]);Con([Num(5)]);Num(4);Con([Num(7);Num(1);Num(9)])])]);Con([Num(5)])]);
Con([Con([Con([Num(10);Con([]);Con([Num(1);Num(4);Num(9);Num(3);Num(9)]);Con([Num(2);Num(3);Num(10);Num(8)]);Con([Num(7);Num(4)])]);Num(6);Num(9)]);Con([Num(10);Num(5);Num(1);Num(2);Con([])]);Con([Con([Con([Num(2);Num(10);Num(7);Num(3)]);Num(4);Num(10)]);Con([Con([Num(4);Num(0);Num(5);Num(0);Num(9)])]);Con([Num(0);Con([Num(9);Num(6);Num(2);Num(10);Num(5)]);Con([]);Num(0)]);Con([Num(3);Con([Num(6);Num(6)])])])]);
Con([Con([Num(5)]);Con([Num(10);Num(6);Con([Num(0);Num(10);Con([Num(4);Num(4);Num(8)]);Num(9)]);Num(2);Con([Con([]);Num(9)])])]);
Con([Con([Num(1);Num(2)])]);
Con([Con([Con([Con([Num(9)]);Con([Num(9);Num(5);Num(1)]);Con([Num(8);Num(6);Num(10);Num(3)]);Con([Num(4);Num(9)])]);Num(3);Con([Con([])])])]);
Con([Con([Num(1)]);Con([Con([])]);Con([Con([Con([Num(7);Num(0)]);Con([Num(8);Num(0);Num(0);Num(0);Num(7)]);Num(8);Con([]);Num(9)])])]);
Con([Con([Num(10);Con([Con([]);Num(3);Num(8)]);Con([]);Con([Con([Num(0);Num(4);Num(2);Num(8);Num(10)]);Con([Num(0);Num(5)]);Con([]);Con([Num(10);Num(3);Num(8);Num(5);Num(9)]);Num(1)])]);Con([Con([Num(10);Con([Num(5)]);Num(7);Num(5);Con([Num(3);Num(7);Num(4)])]);Num(3)]);Con([Con([Con([Num(1);Num(0);Num(2);Num(0);Num(9)])])])]);
Con([Con([Con([]);Num(6);Num(2);Con([Num(8);Con([]);Con([Num(2);Num(7);Num(9)]);Con([])])]);Con([Con([]);Con([])])]);
Con([Con([Con([])]);Con([Num(10);Con([]);Num(4);Con([Num(5);Con([Num(9);Num(4)])])]);Con([Con([Con([]);Con([Num(2);Num(9);Num(0)]);Num(9);Num(3);Num(1)])]);Con([Con([Con([Num(6)])]);Num(7);Con([Con([Num(5);Num(2)]);Con([Num(5);Num(7);Num(9);Num(7);Num(6)]);Con([Num(6);Num(4);Num(2);Num(9)])]);Con([Con([Num(3);Num(7);Num(6);Num(7);Num(10)]);Num(9);Con([Num(8);Num(0);Num(8);Num(0)]);Con([Num(9);Num(4);Num(4)])])])]);
Con([Con([Con([Num(2);Con([Num(7);Num(4);Num(5);Num(1);Num(4)]);Num(9);Con([]);Num(5)]);Con([Num(9);Con([])]);Num(5);Num(0)]);Con([]);Con([Num(3);Con([Num(2);Num(1);Num(10);Con([Num(2)]);Con([Num(5);Num(5);Num(0);Num(4)])]);Num(2);Num(6);Con([Num(10);Num(1);Num(2);Num(1)])]);Con([Con([Num(9)]);Num(8);Con([Num(3);Num(8);Con([Num(8)]);Con([Num(0)]);Num(0)]);Con([Con([Num(0);Num(3);Num(10)]);Num(3);Num(6)])]);Con([Con([Num(4);Con([Num(0);Num(0);Num(10);Num(8);Num(7)])]);Con([]);Con([Con([]);Con([Num(1);Num(5);Num(4)]);Num(1);Con([Num(1);Num(1)])]);Con([Con([Num(9);Num(8);Num(6);Num(5)]);Con([Num(10);Num(5);Num(2);Num(1)]);Con([Num(4);Num(4);Num(0);Num(0)])])])]);
Con([Con([Num(2);Con([Num(9)]);Num(8)]);Con([Con([Num(2)]);Con([Num(0);Num(4);Num(1);Con([Num(3);Num(0);Num(7);Num(3)]);Num(1)]);Num(1);Con([Con([Num(10);Num(4);Num(2);Num(9)]);Con([Num(3);Num(4)]);Con([]);Con([]);Num(1)]);Num(1)]);Con([Con([Num(5)]);Num(2)])]);
Con([Con([Con([]);Num(5);Con([Num(2);Con([Num(4);Num(8);Num(0)]);Con([Num(4);Num(9);Num(0);Num(2)]);Num(9)]);Con([Con([Num(3)]);Con([Num(5);Num(2);Num(2)])]);Con([])]);Con([Con([Num(5);Con([Num(6);Num(10);Num(6);Num(0)])]);Con([Num(5);Con([Num(10);Num(7);Num(9);Num(4);Num(6)]);Con([Num(6)])]);Con([Con([Num(3);Num(4);Num(5);Num(7);Num(5)]);Con([Num(2);Num(0)])]);Num(7);Con([Num(2);Con([]);Num(0);Num(8)])]);Con([Num(2);Con([Con([Num(4)]);Num(3)]);Num(7)])]);
Con([Con([Con([Num(0)])]);Con([Con([Con([Num(2);Num(3);Num(0)])]);Con([Con([])])]);Con([])]);
Con([Con([Con([Con([Num(4);Num(6);Num(3);Num(0)]);Num(1)]);Num(1);Num(9);Con([Num(2)]);Con([Num(9);Con([])])])]);
Con([Con([]);Con([])]);
Con([]);
Con([Con([Con([Num(9);Num(7)]);Con([Num(7);Num(9);Con([Num(8);Num(7);Num(6);Num(0);Num(0)])])])]);
Con([Con([Num(10);Con([Num(3);Num(10);Num(3);Num(6);Con([Num(6);Num(6)])]);Con([Num(7);Con([])]);Num(6)])]);
Con([Con([]);Con([Num(6);Num(1);Con([])])]);
Con([Con([Con([Num(7)]);Con([Con([Num(1);Num(0);Num(6);Num(4)]);Num(9);Con([Num(2);Num(0)]);Num(3)]);Num(0);Con([Num(9);Num(1);Num(9)])]);Con([Con([]);Con([Num(6)]);Num(6);Num(3)]);Con([Num(1);Num(7);Con([Num(4);Con([Num(4)]);Con([Num(2);Num(7);Num(3);Num(4)]);Num(2);Num(5)]);Con([Con([Num(4)]);Con([Num(3);Num(7)])]);Num(1)]);Con([])]);
Con([Con([Con([Num(2);Num(2);Num(4);Num(3);Num(6)]);Con([]);Con([]);Con([Num(9);Con([Num(10);Num(4);Num(0);Num(10)]);Con([Num(6);Num(8);Num(6)])]);Con([Con([])])]);Con([Con([Con([Num(1);Num(10);Num(0);Num(3)]);Con([Num(6);Num(4)]);Num(1);Num(3)]);Num(0);Con([Con([Num(8);Num(6);Num(6)]);Con([Num(7);Num(2)])]);Con([Num(6);Num(8);Num(0);Num(9);Num(7)])]);Con([Num(2);Num(3);Con([Con([Num(3);Num(2);Num(7);Num(6);Num(4)]);Num(3);Num(0);Num(8)])]);Con([Num(10);Con([Num(6);Num(6);Num(1)]);Con([Con([Num(10);Num(1)]);Num(4);Num(5)])]);Con([Num(0);Con([Num(5);Num(2);Con([Num(10);Num(2);Num(9);Num(1)])]);Con([])])]);
Con([Con([Num(2)]);Con([Num(7)])]);
Con([Con([]);Con([Con([Num(0);Con([Num(1);Num(1)])]);Con([Con([Num(5);Num(6);Num(10);Num(8)]);Num(10);Num(7);Con([Num(2);Num(9);Num(2);Num(9)]);Con([Num(1)])])]);Con([Num(5);Num(7);Num(3)]);Con([Con([Con([]);Num(6);Num(2);Num(7)]);Num(6);Con([Num(3);Con([Num(9)]);Con([Num(10)]);Con([Num(5);Num(6);Num(0);Num(2)]);Con([Num(3);Num(3);Num(3);Num(9)])])]);Con([Con([Con([Num(6)]);Num(8);Con([Num(5);Num(7);Num(1);Num(7);Num(3)])]);Num(1);Num(2);Num(0);Con([])])]);
Con([Con([]);Con([Con([Con([Num(7);Num(1);Num(3);Num(5);Num(7)]);Num(1);Num(4);Con([Num(7);Num(9);Num(8);Num(7)]);Num(2)])])]);
Con([Con([Num(4);Num(1);Num(0)]);Con([Con([Con([]);Num(2)]);Con([Num(9);Con([])]);Con([Num(4);Num(9);Num(0);Num(1)]);Con([Con([Num(5);Num(8)]);Con([Num(8);Num(10);Num(5);Num(7)]);Con([Num(2);Num(5)]);Num(8)])]);Con([]);Con([]);Con([Num(0);Con([]);Con([Num(6);Num(4);Num(0)]);Con([])])]);
Con([Con([Num(8)]);Con([Con([])]);Con([Con([Con([]);Num(6)])]);Con([Num(0);Con([]);Num(6);Num(5);Con([])]);Con([])]);
Con([Con([Con([Con([Num(0);Num(4);Num(10);Num(6);Num(9)]);Con([Num(6);Num(2);Num(8);Num(3);Num(0)]);Num(2);Con([Num(2);Num(0);Num(0)])]);Num(4);Num(3);Con([Num(5);Con([])])]);Con([Con([Num(0);Con([Num(5);Num(5);Num(1);Num(10);Num(10)]);Num(4)])]);Con([Num(2);Num(5)]);Con([])]);
Con([Con([]);Con([Con([Con([Num(2);Num(9);Num(6);Num(7)]);Con([]);Con([Num(9)]);Con([])])]);Con([Con([Num(2);Num(3);Con([Num(9);Num(9);Num(8);Num(1);Num(7)])]);Num(1);Num(0);Num(9)]);Con([Num(2)]);Con([Num(0);Con([Num(6);Con([Num(2);Num(8);Num(8);Num(10);Num(8)]);Con([Num(7);Num(6);Num(7)])])])]);
Con([Con([Num(9);Num(3)]);Con([Num(4)]);Con([Con([Num(6);Num(6);Num(9);Con([Num(5);Num(9)]);Num(6)])]);Con([])]);
Con([Con([]);Con([Con([]);Con([Con([Num(10);Num(5);Num(6)]);Num(0);Num(10)]);Con([Con([Num(5);Num(3);Num(8);Num(4);Num(8)]);Num(10);Num(0);Num(1);Con([])]);Num(5)]);Con([Num(10);Num(5)]);Con([Con([Num(9);Con([Num(8);Num(1);Num(4);Num(0)]);Num(0)]);Con([Con([Num(3);Num(0);Num(9)]);Con([Num(3);Num(2);Num(10)]);Num(10);Num(6);Num(8)]);Con([Num(4);Num(2);Num(1)]);Con([Num(5)])]);Con([Num(6);Con([Num(2)]);Num(0);Con([])])]);
Con([Con([Con([Con([Num(0);Num(1);Num(3)]);Con([Num(0);Num(0);Num(2);Num(7)]);Con([])]);Con([]);Num(6);Con([Con([Num(4);Num(6);Num(0);Num(4);Num(4)])]);Num(4)]);Con([Num(10);Num(1);Num(3);Con([Num(2);Con([Num(9);Num(7);Num(0)]);Con([Num(2)]);Num(9)])])]);
Con([Con([]);Con([Con([Num(3);Con([Num(4);Num(3);Num(2)])]);Con([Num(2);Con([Num(5);Num(9);Num(3);Num(4);Num(4)]);Con([])]);Num(2)]);Con([Con([Con([Num(8);Num(8);Num(3);Num(0);Num(7)]);Num(6);Con([]);Num(1);Con([Num(5);Num(2);Num(8);Num(8);Num(6)])]);Num(5)]);Con([Con([Num(10)]);Con([Con([Num(0);Num(0);Num(2);Num(5)])]);Num(1)])]);
Con([Con([Con([Num(3)]);Num(1)]);Con([Num(5)]);Con([Con([Num(0);Con([Num(6);Num(9);Num(7);Num(0)]);Con([])]);Num(0);Num(5);Con([Con([Num(7);Num(6);Num(10);Num(2);Num(4)]);Num(10)])])]);
Con([Con([Num(3);Con([]);Con([Num(0);Con([Num(7)]);Con([Num(5);Num(7)]);Num(6)])])]);
Con([Con([Num(4);Num(3);Num(1)]);Con([Con([Con([Num(0);Num(5)]);Num(1);Con([Num(8);Num(2);Num(9);Num(9)]);Num(3);Num(1)])]);Con([Num(7);Con([Con([Num(6);Num(1);Num(6);Num(0);Num(9)]);Con([Num(0)]);Con([Num(7);Num(6)]);Con([]);Num(7)]);Num(2);Con([Num(9);Num(7);Con([Num(8);Num(9);Num(7);Num(9);Num(4)]);Num(9)])]);Con([Con([Con([Num(9);Num(8);Num(8);Num(7);Num(2)]);Con([Num(5);Num(7);Num(10);Num(9)])]);Con([]);Con([Num(2)])]);Con([])]);
Con([Con([Num(8);Con([Num(0)])]);Con([Num(6);Num(5);Con([]);Con([Num(1);Num(1);Con([Num(7);Num(9);Num(8);Num(5)]);Num(1)])])]);
Con([Con([Con([Con([Num(9)]);Con([Num(10);Num(4);Num(7);Num(0)]);Con([Num(5);Num(6)]);Con([Num(7);Num(3);Num(6);Num(0);Num(10)])])]);Con([]);Con([Con([Con([Num(9);Num(8);Num(10);Num(4);Num(2)]);Con([Num(1);Num(6);Num(1)]);Num(10)]);Num(3)])]);


] in

  let pairs = to_pairs input in

  let p1_idx = List.mapi 
                ~f:(fun i -> fun pair -> 
                      match container_compare (fst pair) (snd pair) with 
                      | Less -> (i+1) 
                      | _ -> 0 ) pairs 
  in

  let div1 = Con([Con([Num(2)])]) in
  let div2 = Con([Con([Num(6)])]) in

  let sorted = List.sort 
                ~compare:(fun l -> fun r -> 
                            match container_compare l r with 
                            | Eq -> 0 
                            | Less -> -1 
                            | Greater -> 1) 
                (div1 :: div2 :: input)
  in

  let p2_idx = List.mapi 
                ~f:(fun i -> fun t ->
                      match t with 
                        | Con([Con([Num(2)])]) 
                        | Con([Con([Num(6)])]) -> i+1 
                        | _                    -> 1 
                   )
               sorted 
  in

  printf "Part 1 answer: %d\n" (List.fold_left ~f:( + ) ~init:0 p1_idx);
  printf "Part 2 answer: %d\n" (List.fold_left ~f:( * ) ~init:1 p2_idx);
