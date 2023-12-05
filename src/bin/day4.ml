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

type data =
  { winning : int list
  ; numbers : int list }

let parse ~(line : string) : data =
  let winning =
    line
    |> substr ~first:(String.index line ':' + 1) ~last:(String.index line '|')
    |> String.split_on_char ' '
    |> List.filter_map (function
      | "" -> None
      | s -> Some (int_of_string s))
  in
  let numbers =
    line
    |> substr ~first:(String.index line '|' + 1)
    |> String.split_on_char ' '
    |> List.filter_map (function
      | "" -> None
      | s -> Some (int_of_string s))
  in
  {winning; numbers}

let part_1 ~(data: string list) : int =
  let next_pow2 acc contains =
    if contains then match acc with
    | 0 -> 1
    | n -> n * 2
    else acc
  in
  data
  |> sum_map (fun line ->
    let {winning; numbers} = parse ~line in
    numbers
    |> fold_map ~init:0 ~op:next_pow2 (fun el ->
        List.mem el winning
    )
  )

let winning_count ({winning; numbers} : data) : int =
  numbers |> sum_map (fun el -> if List.mem el winning then 1 else 0)

let part_2 ~(data: string list) : int =
  let counts = Array.make (List.length data) 1 in
  data
  |> List.iteri (fun idx line ->
    let el = parse ~line in
    let count = winning_count el in
    let to_add = counts.(idx) in
    let last = if idx + count < Array.length counts then
        idx + count
      else
        Array.length counts - 1
    in
    if count <> 0 then
      for i = idx + 1 to last do
        counts.(i) <- counts.(i) + to_add
      done
  ) ;
  Array.fold_left (+) 0 counts

let () = main ~part_1 ~part_2
