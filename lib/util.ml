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

let int_of_digit (c : char) : int option =
  if '0' <= c && c <= '9' then
    Some ((Char.code c) - (Char.code '0'))
  else None

let substr ~(first:int) ?(last:int option) (s: string) : string=
  let last = match last with
  | Some l -> if l < 0 then String.length s - l - 1 else l
  | None -> String.length s
  in
  String.sub s first (last - first)

let concat_mapi (f : int -> 'a -> 'b list) (lst : 'a list) : 'b list =
  let rec recurse idx acc = function
    | [] -> acc
    | a :: rest -> recurse (idx + 1) (acc @ (f idx a)) rest
  in
  recurse 0 [] lst

let main
  ~(part_1 : data:(string list) -> int)
  ~(part_2 : data:(string list) -> int)
  : unit
=
  let read_in_file (s : string) : string list =
    let f = open_in s in
    let rec recurse acc = match input_line f with
      | s -> recurse (s :: acc)
      | exception End_of_file -> acc
    in
    let result = recurse [] in
    close_in f ;
    result
  in
  let part = int_of_string (Array.get Sys.argv 1) in
  let data = read_in_file (Array.get Sys.argv 2) in
  let result = match part with
  | 1 -> part_1 ~data
  | 2 -> part_2 ~data
  | _ -> invalid_arg (string_of_int part)
  in
  Format.printf "%d\n" result
