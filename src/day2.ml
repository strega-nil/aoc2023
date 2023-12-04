open Util

type count =
  { red : int
  ; green : int
  ; blue : int }

type game =
  { id : int
  ; counts : count list }

let subgame_counts (subgame: string) : count =
  let counts_data = String.split_on_char ',' subgame in
  let parse_field_as_color field color =
    if String.ends_with ~suffix:color field then
      field
      |> substr ~first:1 ~last:(String.length field - String.length color - 1)
      |> int_of_string
      |> function x -> Some x
    else
      None
  in
  let parse_field acc field =
    match parse_field_as_color field "red" with
    | Some red -> { acc with red }
    | None ->
    match parse_field_as_color field "green" with
    | Some green -> { acc with green }
    | None ->
    match parse_field_as_color field "blue" with
    | Some blue -> { acc with blue }
    | None -> invalid_arg field
  in
  counts_data
  |> List.fold_left parse_field {red = 0; green = 0; blue = 0}

let parse_game (line : string) : game =
  let colon_idx = String.index line ':' in
  let subgames = line
    |> substr ~first:(colon_idx + 1)
    |> String.split_on_char ';'
  in
  let counts = subgames |> List.map subgame_counts in
  let id =
    (* "Game N...: ..."
        012345
     *)
    line
    |> substr ~first:5 ~last:colon_idx
    |> int_of_string
  in
  {id; counts}

let part_1 ~(data : string list) : int =
  let max = { red = 12; green = 13; blue = 14 } in
  let is_valid_subgame (count : count) : bool =
    count.red <= max.red && count.green <= max.green && count.blue <= max.blue
  in
  let valid_game_id (line : string) : int option =
    let {id; counts} = parse_game line in
    if List.for_all is_valid_subgame counts then
      Some id
    else
      None
  in
  data
    |> List.filter_map valid_game_id
    |> List.fold_left (+) 0

let part_2 ~(data : string list) : int =
  let (^^) (lhs : int) (rhs : int) = if lhs > rhs then lhs else rhs in
  let combine (lhs : count) (rhs : count) : count =
    {red = lhs.red ^^ rhs.red
    ; green = lhs.green ^^ rhs.green
    ; blue = lhs.blue ^^ rhs.blue}
  in
  data
  |> List.map (fun line ->
    let {counts; _} = parse_game line in
    let blocks = counts
      |> List.fold_left combine {red = 0; green = 0; blue = 0}
    in
    blocks.red * blocks.green * blocks.blue
  )
  |> List.fold_left (+) 0

let () = Util.main ~part_1 ~part_2
