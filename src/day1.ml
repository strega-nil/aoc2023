let part_1 ~(data : string list) : int =
  let parse_line (s : string) : int =
    let is_digit (c : char) : bool = '0' <= c && c <= '9' in
    let digit_value (c : char) : int = (Char.code c) - (Char.code '0') in

    let len = String.length s in
    let rec recurse (fst : int option) (lst : int option) (idx : int) =
      if idx = len then ((Option.get fst), (Option.get lst))
      else
        let elt = s.[idx] in
        if is_digit elt then
          let value = digit_value elt in
          recurse (Some (Option.value ~default:value fst)) (Some value) (idx + 1)
        else
          recurse fst lst (idx + 1)
    in
    let (a, b) = recurse None None 0 in
    a * 10 + b
  in
  data
  |> List.fold_left (fun acc line -> acc + parse_line line) 0

let part_2 ~(data : string list) : int =
  let parse_line (s : string) : int =
    let to_digit (s : string) (i: int) : int option =
      let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
      let names = ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in
      let fst = String.get s i in
      match List.find_index (function el -> fst = el) digits with
      | Some i -> Some i
      | None ->
        List.find_index
          (function el -> let open String in (length s - i) >= length el && el = sub s i (length el))
          names
    in

    let len = String.length s in
    let rec recurse (fst : int option) (lst : int option) (idx : int) =
      if idx = len then ((Option.get fst), (Option.get lst))
      else
        match to_digit s idx with
        | Some value ->
          recurse (Some (Option.value ~default:value fst)) (Some value) (idx + 1)
        | None -> recurse fst lst (idx + 1)
    in
    let (a, b) = recurse None None 0 in
    a * 10 + b
  in
  data
  |> List.fold_left (fun acc line -> acc + parse_line line) 0

let () = Util.main ~part_1 ~part_2
