

let get_key (k, _) =
  k
let get_values (_, v) =
  List.fold_left (fun acc v -> acc ^ v ^ ",") "" v

let rec print_query = function
  | h :: tl -> Printf.printf "key=%s value=[%s]\n" (get_key h) (get_values h) ; print_query tl
  | [] -> ()

let path_elements path =
  path |> Str.split (Str.regexp "/") |> List.filter (fun s -> String.length s > 0)

let uri_query uri =
  print_endline uri ;
  let query = uri |> Uri.of_string |> Uri.query in
  print_query query ;
  Yojson.Safe.(`String "uri hello")
