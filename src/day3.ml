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
  | Number of int
  | Symbol of char

type cell = 
  { x : int
  ; y : int 
  ; data : data }

let floor_log_10 (x : int) =
  let rec recurse acc cur =
    if cur < 10 then acc
    else recurse (acc + 1) (cur / 10)
  in
  recurse 0 x

let last_x (c : cell) : int =
  match c.data with
  | Symbol _ -> c.x
  | Number i -> c.x + floor_log_10 i

let adjacent_cells (c: cell) (cells : cell list) : cell list =
  cells |> List.filter (fun c' ->
    let within_x =
      (c.x - 1 <= c'.x && c'.x <= (last_x c) + 1)
      || (c'.x - 1 <= c.x && c.x <= (last_x c') + 1)
    in
    let within_y = (c.y - 1 <= c'.y && c'.y <= c.y + 1) in
    within_x && within_y
  )

let print_cell_ln (c : cell) : unit =
  Format.printf "{x = %d; y = %d; data = " c.x c.y ;
  match c.data with
  | Number i -> Format.printf "data = Number %d}\n" i
  | Symbol c -> Format.printf "data = Symbol %c}\n" c

let parse ~(data: string list) : cell list =
  let rec parse_number_from_line (idx : int) (acc : int) (line : string) : int * int =
    if idx = String.length line then idx, acc
    else match int_of_digit (String.get line idx) with
    | Some d -> parse_number_from_line (idx + 1) (acc * 10 + d) line
    | None -> idx, acc
  in
  let rec parse_line (x : int) (y : int) (acc : cell list) (line : string) : cell list =
    if x = String.length line then acc
    else
    let c = String.get line x in
    if c = '.' then parse_line (x + 1) y acc line
    else match int_of_digit c with
    | None -> parse_line (x + 1) y ({x; y; data = Symbol c} :: acc) line
    | Some n -> let x', n = parse_number_from_line (x + 1) n line in
      parse_line x' y ({x; y; data = Number n} :: acc) line
  in
  concat_mapi (fun y el -> (parse_line 0 y [] el)) data


let part_1 ~(data: string list) : int =
  let cells = parse ~data in
  cells |> List.fold_left (fun acc el ->
    match el.data with
    | Number i -> let has_adjacent_symbol =
        (adjacent_cells el cells)
        |> List.exists (function | {data = Symbol _; _} -> true | _ -> false)
      in 
      if has_adjacent_symbol then acc + i
      else acc
    | _ -> acc
  ) 0

let part_2 ~(data: string list) : int =
  let cells = parse ~data in
  cells |> List.fold_left (fun acc el ->
    match el.data with
    | Symbol '*' -> begin
        let adjacent_nums =
          adjacent_cells el cells
          |> List.filter (function | {data = Number _; _} -> true | _ -> false)
        in
        match adjacent_nums with
        | [{data = Number fst; _}; {data = Number snd; _}] -> acc + (fst * snd)
        | _ -> acc
      end
    | _ -> acc
  ) 0

let () = main ~part_1 ~part_2
