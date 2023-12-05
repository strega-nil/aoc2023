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

type 'a t = 
  { data : 'a array (* data.length = width * height *)
  ; width : int
  ; height : int }

let make (width : int) (height : int) ~(default : 'a) =
  let data = Array.make (width * height) default in
  { data ; width ; height }

let make_internal_index self x y =
  if x > self.width then (invalid_arg (string_of_int x))
  else if y > self.height then (invalid_arg (string_of_int y))
  else y * self.width + x

module Operators = struct
  let (.%( )) (self : 'a t) ((x, y) : int * int) =
    self.data.(make_internal_index self x y)

  let (.%( )<-) (self : 'a t) ((x, y) : int * int) (value : 'a) =
    self.data.(make_internal_index self x y) <- value
end
open Operators

let fold_left_i (acc : 'acc) (f : (int -> int -> 'acc -> 'a -> 'acc)) (self : 'a t) : 'acc =
  let rec recurse x y acc =
    let acc = f x y acc self.%(x, y) in
    if x = self.width - 1 then
      if y = self.height - 1 then
        acc
      else
        recurse 0 (y + 1) acc
    else
      recurse (x + 1) y acc
  in
  if self.width = 0 || self.height = 0 then acc
  else recurse 0 0 acc
