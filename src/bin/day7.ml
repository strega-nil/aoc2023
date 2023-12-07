(*
Copyright (C) 2023 Nicole Mazzuca

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Util

type card =
  | Digit of int
  | Ten
  | Jack
  | Queen
  | King
  | Ace

let string_of_card : card -> string = function
  | Digit d -> string_of_int d
  | Ten -> "T"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

type hand =
  Hand of card list

let string_of_hand (Hand h : hand) : string =
  h |> List.map string_of_card |> String.concat ""

type hand_type =
  | High_card
  | Pair
  | Two_pair
  | Three_of
  | Full_house
  | Four_of
  | Five_of

let string_of_hand_type : hand_type -> string = function
  | High_card -> "high card"
  | Pair -> "pair"
  | Two_pair -> "two pair"
  | Three_of -> "three of a kind"
  | Full_house -> "full house"
  | Four_of -> "four of a kind"
  | Five_of -> "five of a kind"


module type Scoring = sig
  val compare_card : card -> card -> int
  val hand_type : hand -> hand_type
end

let compare_hand (s : (module Scoring)) (lhs : hand) (rhs : hand) : int =
  let module S = (val s) in
  let lhs_type = S.hand_type lhs in
  let rhs_type = S.hand_type rhs in
  let type_cmp = compare lhs_type rhs_type in
  if type_cmp != 0 then type_cmp
  else
    let Hand lhs, Hand rhs = lhs, rhs in
    List.compare S.compare_card lhs rhs



module Jack : Scoring = struct
  let compare_card (lhs : card) (rhs : card) : int =
    if lhs = rhs then 0
    else match lhs, rhs with
    | Digit ln, Digit rn -> compare ln rn
    | Digit _, _ -> -1
    | _, Digit _ -> 1
    | Ten, _ -> -1
    | _, Ten -> 1
    | Jack, _ -> -1
    | _, Jack -> 1
    | Queen, _ -> -1
    | _, Queen -> 1
    | King, Ace -> -1
    | Ace, King -> 1
    | _ -> failwith "unreachable"

  let hand_type (Hand h : hand) : hand_type =
    let counts = List.map (fun el -> count_of el h) h in
    let max = find_max ~lt:(<) counts in
    match max with
    | 5 -> Five_of
    | 4 -> Four_of
    | 1 -> High_card
    | 3 ->
      if List.mem 2 counts then
        Full_house
      else 
        Three_of
    | 2 ->
      if count_of 2 counts > 2 then
        Two_pair
      else
        Pair
    | _ -> invalid_arg "hand_type"
end

module Joker : Scoring = struct
  let compare_card (lhs : card) (rhs : card) : int =
    if lhs = rhs then 0
    else match lhs, rhs with
    | Jack, _ -> -1
    | _, Jack -> 1
    | Digit ln, Digit rn -> compare ln rn
    | Digit _, _ -> -1
    | _, Digit _ -> 1
    | Ten, _ -> -1
    | _, Ten -> 1
    | Queen, _ -> -1
    | _, Queen -> 1
    | King, Ace -> -1
    | Ace, King -> 1
    | _ -> failwith "unreachable"

  let hand_type (Hand h : hand) : hand_type =
    let counts =
      h
      |> List.map (fun el -> count (fun c -> c = el || c = Jack) h)
    in
    let max = counts |> find_max ~lt:(<) in
    match max with
    | 5 -> Five_of
    | 4 -> Four_of
    | 1 -> High_card
    | 3 ->
        let count_of_j = count_of Jack h in
        if count_of_j = 2 then
          (* J J a b c *)
          Three_of
        else if count_of_j = 1 then
          (* J a a b b|c *)
          if count_of 3 counts = 4 then
            Full_house
          else
            Three_of
        else (* count_of_j = 0, since otherwise we'd be in the 4/5 case *)
          if List.mem 2 counts then
            Full_house
          else
            Three_of
    | 2 ->
        if List.mem Jack h then
          (* J a b c d *)
          Pair
        else (* a a b b|c d *)
          if count_of 2 counts = 4 then
            Two_pair
          else
            Pair
    | _ -> invalid_arg "hand_type"
end
  


let parse ~(data: string list) : (hand * int) list =
  let parse_line (line : string) : hand * int =
    match String.split_on_char ' ' line with
    | [hand; bet] ->
        let bet = int_of_string bet in
        let hand_lst = ref [] in
        hand |> String.iter (fun c ->
          let card = match int_of_digit c with
          | Some i -> Digit i
          | None -> match c with
            | 'T' -> Ten
            | 'J' -> Jack
            | 'Q' -> Queen
            | 'K' -> King
            | 'A' -> Ace
            | _ -> invalid_arg "parse_line"
          in
          hand_lst := card :: !hand_lst)
        ;
        let hand_lst = List.rev (!hand_lst) in
        Hand hand_lst, bet
    | _ -> invalid_arg "parse"
  in
  List.map parse_line data

let part_1 ~(data: string list) : int =
  let hands =
    parse ~data
    |> List.sort (fun (h1, _) (h2, _) -> compare_hand (module Jack) h1 h2)
  in
  hands
  |> fold_mapi ~init:0 ~op:(+) (fun idx (_, bet) -> (idx + 1) * bet)

let part_2 ~(data: string list) : int =
  let hands =
    parse ~data
    |> List.sort (fun (h1, _) (h2, _) -> compare_hand (module Joker) h1 h2)
  in
  hands
  |> fold_mapi ~init:0 ~op:(+) (fun idx (_, bet) -> (idx + 1) * bet)

let () = main ~part_1 ~part_2
