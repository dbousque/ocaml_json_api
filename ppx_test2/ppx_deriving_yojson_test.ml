

type currency = Euro | Dollar [@@deriving yojson]

type account = {
	amount: int ;
	currency: currency
} [@@deriving yojson]

type eu_account = account [@@deriving yojson]

let () =
  let acc = { amount = 12 ; currency = Euro } in
  print_endline (acc |> eu_account_to_yojson |> Yojson.Safe.to_string)