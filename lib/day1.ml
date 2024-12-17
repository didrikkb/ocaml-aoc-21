let max_int = 4611686018427387903

type t =
  | Number of int
  | NewLine

let collect_num input =
  let rec collect_num input res =
    match input with
    | [] -> res, input
    | n :: t ->
      (match n with
       | '0' .. '9' -> collect_num t ((res * 10) + (Char.code n - Char.code '0'))
       | _ -> res, input)
  in
  collect_num input 0
;;

let tokenize input =
  let input = input |> String.to_seq |> List.of_seq in
  let rec tokenize input =
    match input with
    | [] -> []
    | c :: t ->
      (match c with
       | '\n' -> [ NewLine ] @ tokenize t
       | '0' .. '9' ->
         let n, t = collect_num ([ c ] @ t) in
         [ Number n ] @ tokenize t
       | _ -> tokenize t)
  in
  tokenize input
;;

let result_a tokens =
  let rec calculate tokens prev =
    match tokens with
    | [] -> 0
    | token :: rest ->
      (match token with
       | Number n ->
         (match n > prev with
          | true -> 1
          | false -> 0)
         + calculate rest n
       | NewLine -> calculate rest prev)
  in
  calculate tokens max_int
;;

let result_b tokens =
  let rec calculate tokens prev a b idx =
    match tokens with
    | [] -> 0
    | token :: rest ->
      (match token with
       | Number n ->
         let sum = a + b + n in
         (match sum > prev with
          | true when idx > 2 -> 1
          | _ -> 0)
         + calculate rest sum b n (idx + 1)
       | NewLine -> calculate rest prev a b idx)
  in
  calculate tokens 0 0 0 0
;;

let a input = tokenize input |> result_a
let b input = tokenize input |> result_b
