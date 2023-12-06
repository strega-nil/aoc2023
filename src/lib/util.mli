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

val int_of_digit : char -> int option
val substr : first:int -> ?last:int -> string -> string
val concat_mapi : (int -> 'a -> 'b list) -> 'a list -> 'b list
val fold_map : init:'acc -> op:('acc -> 'b -> 'acc) -> ('a -> 'b) -> 'a list -> 'acc
val find_min : lt:('a -> 'a -> bool) -> 'a list -> 'a
val find_max : lt:('a -> 'a -> bool) -> 'a list -> 'a
val sum_map : ('a -> int) -> 'a list -> int
val main : part_1:(data:(string list) -> int) -> part_2:(data:(string list) -> int) ->  unit
