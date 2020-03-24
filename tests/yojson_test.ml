

type state = Ok | Error of (string * int) [@@deriving yojson]
type lolz = { id: int option ; name: string ; opts: state list } [@@deriving yojson]

let () =
  let to_yojson = [%derive.to_yojson:lolz] in
  let opts = [Ok ; Error ("hello", 34)] in
  let value = { id = Some 12 ; name = "Dominik" ; opts = opts } in
  let json = to_yojson value in
  let str = Yojson.Safe.to_string json in
  print_endline str