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

type race = 
  { total_time : int
  ; record_dist : int }

let parse_1 ~(data:string list) : race list =
  match data with
  | [time; distance] ->
      let parse_line s =
        s
        |> substr ~first:((String.index s ':') + 1)
        |> String.split_on_char ' '
        |> List.filter_map (fun s -> if String.length s = 0 then None else Some (int_of_string s))
      in
      List.map2 (fun total_time record_dist -> {total_time; record_dist}) (parse_line time) (parse_line distance)
  | _ -> failwith "unexpected input"

let parse_2 ~(data: string list) : race =
  match data with
  | [time; distance] ->
      let parse_line s =
        s
        |> substr ~first:((String.index s ':') + 1)
        |> String.split_on_char ' '
        |> String.concat ""
        |> int_of_string
      in
      {total_time = (parse_line time); record_dist = (parse_line distance)}
  | _ -> failwith "unexpected input"

(*
  Let V(t) be the velocity given that the button has been held for h milliseconds.
  Let D_t(h) be the distance that a boat will go in t milliseconds,
  given that the button has been held for h milliseconds.

  Let R_t be the record distance for that amount of time.

  We want all h s.t. D_t(h) > R_t
  
  Note:
    D_t(h) = (t - h) V(h)
      = (t - h) h
      = th - h^2

  D_t(h)          > R_t
  th - h^2        > R_t
  -h^2 + th - R_t > 0

  solve the LHS:

  h = \frac {-b \pm \sqrt{b^2 - 4ac}}{2a}
    = \frac {-t \pm \sqrt{t^2 - 4R_t}}{-2}
    = \frac {t \pm \sqrt{t^2 - 4R_t)}}{2}

  We want h \in \left( \frac{t - \sqrt{t^2 - 4R_t}}{2}, \frac{t + \sqrt{t^2 - 4R_t}}{2} \right)
 *)

(* 
  let min, max = result in
    h \in [min, max)
  *)
let h_range ({total_time= t; record_dist= r_t}: race) : int * int =
  let t_flt = float_of_int t in
  let r_t_flt = float_of_int r_t in
  let min_flt = 
    (t_flt -. sqrt(t_flt *. t_flt -. 4. *. r_t_flt)) /. 2.
  in
  let max_flt = 
    (t_flt +. sqrt(t_flt *. t_flt -. 4. *. r_t_flt)) /. 2.
  in
  (int_of_float min_flt) + 1, (int_of_float (ceil max_flt))

let part_1 ~(data: string list) : int =
  let races = parse_1 ~data in
  races
  |> fold_map ~init:1 ~op:( * ) (fun r -> let min, max = h_range r in max - min)

let part_2 ~(data: string list) : int =
  let min, max = h_range (parse_2 ~data) in
  max - min

let () = main ~part_1 ~part_2
