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
      match
        set
        |> List.find_opt (fun map ->
          map.source_start <= cur && cur < map.source_start + map.length)
      with
      | Some map -> map.dest_start + (cur - map.source_start)
      | None -> cur
    ) seed
  in
  let min a b = if a < b then a else b in
  seeds
  |> fold_map ~init:max_int ~op:min find_final

let part_2 ~(data: string list) : int =
  let seed_ranges, sets = parse ~data in
  (* I need to be more clever here than just building up the list of seeds *)
  failwith "unimplemented"

let () = main ~part_1 ~part_2
