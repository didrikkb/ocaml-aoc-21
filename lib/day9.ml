let find_low_point data =
  let x_len, y_len = Array.length data, Array.length data.(0) in
  Array.mapi (fun i x -> i, x) data
  |> Array.fold_left
       (fun acc x ->
         let i, l = x in
         let t =
           Array.mapi (fun j y -> j, y) l
           |> Array.fold_left
                (fun acc x ->
                  let j, curr = x in
                  match
                    ( i = 0 || data.(i - 1).(j) > curr
                    , i = x_len - 1 || data.(i + 1).(j) > curr
                    , j = 0 || data.(i).(j - 1) > curr
                    , j = y_len - 1 || data.(i).(j + 1) > curr )
                  with
                  | true, true, true, true -> (curr + 1) :: acc
                  | _ -> acc)
                []
         in
         t @ acc)
       []
;;

let a _ =
  Core.In_channel.read_lines "data9.txt"
  |> List.to_seq
  |> Seq.map (fun s ->
    List.init (String.length s) (String.get s)
    |> List.map (fun c -> int_of_char c - int_of_char '0')
    |> Array.of_list)
  |> Array.of_seq
  |> find_low_point
  |> List.fold_left (fun acc x -> acc + x) 0
;;
