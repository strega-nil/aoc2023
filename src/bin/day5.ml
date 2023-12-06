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

type map =
  { source_start : int
  ; dest_start : int
  ; length : int }

type set =
  { set : map list }

let in_map (el : int) (m : map) : bool =
  m.source_start <= el && el < m.source_start + m.length

let parse ~(data : string list) : int list * set list =
  match data with
  | [] -> invalid_arg "empty data"
  | seeds :: rest ->
    let seeds =
      String.split_on_char ' ' seeds
      |> List.tl 
      |> List.map int_of_string
    in
    let rec recurse sets cur_set = function
      | [] -> {set = List.rev cur_set} :: sets
      | "" :: rest -> recurse sets cur_set rest
      | line :: rest ->
          if String.ends_with ~suffix:"map:" line then
            if cur_set = [] then
              recurse sets [] rest
            else
              recurse ({set = List.rev cur_set} :: sets) [] rest
          else
            let map =
              String.split_on_char ' ' line
              |> List.map int_of_string
            in
            let map = match map with
            | [dest_start; source_start; length] -> {source_start; dest_start; length}
            | _ -> failwith "Unexpected input"
            in
            recurse sets (map :: cur_set) rest
    in
    (seeds, List.rev (recurse [] [] rest))

let part_1 ~(data: string list) : int =
  let seeds, sets = parse ~data in
  let find_final seed =
    sets
    |> List.fold_left (fun cur {set} ->
      match set |> List.find_opt (in_map cur) with
      | Some map -> map.dest_start + (cur - map.source_start)
      | None -> cur
    ) seed
  in
  let min a b = if a < b then a else b in
  seeds
  |> fold_map ~init:max_int ~op:min find_final

type range =
  { start : int
  ; length : int }

let next_map_after (el : int) ({set} : set) : map option =
  set |> List.fold_left
    (fun acc m ->
      if m.source_start <= el then acc
      else match acc with
      | None -> Some m
      | Some m' -> if m.source_start < m'.source_start then Some m else acc)
    None

let map_range (map_set : set) (r : range) : range list =
  let last = r.start + r.length in
  let rec recurse acc first =
    match List.find_opt (in_map first) map_set.set with
    | Some m ->
        let map_last = m.source_start + m.length in
        let start = m.dest_start + (first - m.source_start) in
        if map_last >= last then
          {start; length = last - first} :: acc
        else
          recurse ({start; length = map_last - first} :: acc) map_last
    | None ->
        match next_map_after first map_set with
        | None ->
            {start = first; length = last - first} :: acc
        | Some m ->
            recurse ({start = first; length = m.source_start - first} :: acc) m.source_start
  in
  recurse [] r.start

let part_2 ~(data: string list) : int =
  let seed_ranges, sets = parse ~data in
  let seed_ranges =
    let rec pairify acc = function
      | [] -> acc
      | start :: length :: rest -> pairify ({start; length} :: acc) rest
      | _ -> failwith "unexpected input"
    in
    List.rev (pairify [] seed_ranges)
  in
  let soil_ranges =
    sets
    |> List.fold_left
      (fun ranges set -> ranges |> List.concat_map (map_range set))
      seed_ranges
  in
  let final_min_range =
    soil_ranges |> find_min ~lt:(fun a b -> a.start < b.start)
  in
  final_min_range.start

let () = main ~part_1 ~part_2
