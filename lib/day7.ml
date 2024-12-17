open Streaming

let unwrap = function
  | Some x -> x
  | None -> failwith "lol"
;;

let median nums =
  let len = List.length nums in
  let rec loop l idx =
    match l, idx with
    | x :: _, 0 -> Some x
    | _ :: t, x -> loop t (x - 1)
    | _ -> None
  in
  loop nums (len / 2)
;;

let a data =
  let nums =
    String.split_on_char ',' data
    |> Stream.of_list
    |> Stream.map (fun s -> int_of_string s)
    |> Stream.to_list
    |> List.sort (fun x y -> compare x y)
  in
  let median = median nums |> unwrap in
  List.map (fun x -> Int.abs (median - x)) nums
  |> List.fold_left (fun acc x -> acc + x) 0
;;

let m = ref None

let rec sum_total x = function
  | [] -> 0
  | y :: t -> (unwrap !m).(Int.abs (x - y)) + sum_total x t
;;

let b data =
  let nums =
    String.split_on_char ',' data |> List.map (fun s -> int_of_string s)
  in

  let max = List.fold_left (fun acc x -> Int.max acc x) Int.min_int nums in
  let min = List.fold_left (fun acc x -> Int.min acc x) Int.max_int nums in
  m := Some (Array.init (max - min + 1) (fun _ -> 0));
  Array.mapi_inplace
    (fun i _ -> if i > 0 then i + (unwrap !m).(i - 1) else 0)
    (unwrap !m);
  let rec loop res idx =
    match idx > max with
    | true -> res
    | false ->
      let t = sum_total idx nums in
      loop (Int.min res t) (idx + 1)
  in
  loop Int.max_int min
;;
