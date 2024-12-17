let cti ch = Char.code ch - Char.code '0'

let reduce f l =
  match l with
  | [] -> []
  | h :: t -> List.fold_left f h t
;;

let binary_to_int l =
  let rec helper l res =
    match l with
    | [] -> res
    | h :: t ->
      let res = res lsl 1 in
      let res = if h > 0 then res lor 1 else res in
      helper t res
  in
  helper l 0
;;

let create_gamma_filter l =
  let rec helper l filter =
    match l with
    | [] -> filter
    | _ :: t ->
      let filter = filter lsl 1 in
      let filter = filter lor 1 in
      helper t filter
  in
  helper l 0
;;

let parse input =
  String.split_on_char '\n' input
  |> List.filter (fun x -> String.length x > 0)
  |> List.map (fun x -> List.init (String.length x) (String.get x))
;;

let most_frequent_bits list =
  list
  |> List.map (fun l -> List.map (fun x -> if cti x > 0 then 1 else -1) l)
  |> reduce (fun acc x ->
    let rec h l0 l1 =
      match l0, l1 with
      | h0 :: t0, h1 :: t1 -> [ h0 + h1 ] @ h t0 t1
      | _ -> []
    in
    h acc x)
;;

let a input =
  let gamme_rate_counts = parse input |> most_frequent_bits in
  let gamma_filter = create_gamma_filter gamme_rate_counts in
  let gamma_rate = binary_to_int gamme_rate_counts in
  let epsilon_rate = lnot gamma_rate land gamma_filter in
  gamma_rate * epsilon_rate
;;

let get_oxygen_rating bits freq =
  let rec helper bits freq idx =
    match bits with
    | [] -> failwith "Error"
    | [ x ] -> x
    | _ ->
      let fr = List.nth freq idx in
      let bits =
        List.filter
          (fun bit ->
            let bit = List.nth bit idx in
            fr >= 0 = (cti bit = 1))
          bits
      in
      helper bits (most_frequent_bits bits) (idx + 1)
  in
  helper bits freq 0
;;

let get_co2_rating bits freq =
  let rec helper bits freq idx =
    match bits with
    | [] -> failwith "Error"
    | [ x ] -> x
    | _ ->
      let fr = List.nth freq idx in
      let bits =
        List.filter
          (fun bit ->
            let bit = List.nth bit idx in
            fr >= 0 != (cti bit = 1))
          bits
      in
      helper bits (most_frequent_bits bits) (idx + 1)
  in
  helper bits freq 0
;;

let b input =
  let input = parse input in
  let bit_freq = most_frequent_bits input in
  let oxygen_rating =
    get_oxygen_rating input bit_freq |> List.map (fun x -> cti x)
  in
  let co2_rating = get_co2_rating input bit_freq |> List.map (fun x -> cti x) in
  binary_to_int oxygen_rating * binary_to_int co2_rating
;;
