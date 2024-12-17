type movement =
  | Up of int
  | Down of int
  | Forward of int
  | End

type t =
  { input : string
  ; org_size : int
  }

let init input = { input; org_size = String.length input }

let get_char s idx =
  match String.length s > idx with
  | false -> None
  | true -> Some s.[idx]
;;

let remove_start t idx =
  match String.length t.input >= idx with
  | false -> None
  | true ->
    Some
      { input = String.sub t.input idx (String.length t.input - idx)
      ; org_size = t.org_size
      }
;;

let unwrap o =
  match o with
  | Some v -> v
  | None -> failwith "Attempted to unwrap None!"
;;

let char_to_int ch =
  match ch with
  | '0' .. '9' -> Char.code ch - Char.code '0'
  | _ -> failwith "error no num"
;;

let find_dir t =
  if String.length t.input = 0
  then End, None
  else if String.starts_with t.input ~prefix:"forward"
  then (
    let value = get_char t.input 8 |> unwrap |> char_to_int in
    Forward value, remove_start t 9)
  else if String.starts_with t.input ~prefix:"down"
  then (
    let value = get_char t.input 5 |> unwrap |> char_to_int in
    Down value, remove_start t 6)
  else if String.starts_with t.input ~prefix:"up"
  then (
    let value = get_char t.input 3 |> unwrap |> char_to_int in
    Up value, remove_start t 4)
  else
    failwith
      (Printf.sprintf
         "None valid char on position %d"
         (t.org_size - String.length t.input))
;;

let skip_whitespace t =
  let rec skip idx =
    match get_char t.input idx with
    | None -> None
    | Some ch ->
      (match ch with
       | '\n' | ' ' -> skip (idx + 1)
       | _ -> remove_start t idx)
  in
  skip 0
;;

let next_token t : movement * t option =
  match skip_whitespace t with
  | None -> End, None
  | Some t -> find_dir t
;;

let a input =
  let t = init input in
  let rec simulate t hor vert =
    let dir, t = next_token t in
    let h, v =
      match dir with
      | Up n -> hor - n, vert
      | Down n -> hor + n, vert
      | Forward n -> hor, vert + n
      | End -> hor, vert
    in
    match t with
    | Some t -> simulate t h v
    | None -> h, v
  in
  simulate t 0 0 |> fun (a, b) -> a * b
;;

let b input =
  let t = init input in
  let rec simulate t hor vert aim =
    let dir, t = next_token t in
    let h, v, a =
      match dir with
      | Up n -> hor, vert, aim - n
      | Down n -> hor, vert, aim + n
      | Forward n -> hor + n, vert + (aim * n), aim
      | End -> hor, vert, aim
    in
    match t with
    | Some t -> simulate t h v a
    | None -> h, v
  in
  simulate t 0 0 0 |> fun (a, b) -> a * b
;;
