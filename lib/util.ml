let substr ~(first:int) ?(last:int option) (s: string) =
  let last = match last with
  | Some l -> if l < 0 then String.length s - l - 1 else l
  | None -> String.length s
  in
  String.sub s first (last - first)

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
