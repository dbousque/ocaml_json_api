

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

type currency = Euro | Dollar
type account_status = Blocked of string | Open

type account = {
  id: int ;
  balance: int ;
  balance_scale: int ;
  currency: currency ;
  status: account_status
}

let account_to_string account =
  let balance_int_up = account.balance / (pow 10 account.balance_scale) in
  let balance_int_down = account.balance - (balance_int_up * (pow 10 account.balance_scale)) in
  let balance_string = (string_of_int balance_int_up) ^ "." ^ (Printf.sprintf "%02d" balance_int_down) ^ (match account.currency with | Euro -> "€" | Dollar -> "$") in
  `Assoc [
    ("id", `Int account.id) ;
    ("balance", `String balance_string) ;
    ("status", `String (match account.status with | Blocked str -> "blocked : " ^ str | Open -> "open"))
  ]

let random_account () =
  {
    id = Random.int 5000 ;
    balance = (Random.int 20000) + 1000 ;
    balance_scale = 2 ;
    currency = (match Random.int 2 with | 0 -> Euro | _ -> Dollar) ;
    status = (match Random.int 2 with | 0 -> Blocked "lol" | _ -> Open)
  }

[@@route GET]
module GetUserAccounts = struct
  (* LATER let meth = Method.GET *)
  let path = "/users/<userid:int>/accounts"

  type query = {
    (* LATER page: int [@default 0] ; *)
    per_page: int option
  }

  type body = string list

  type output = (string * account) list

  let validate path_params query body session =
    (* OK | Error "invalid page param" *)
    true

  let handle path_params query body session =
    let per_page = (match query.per_page with | Some x -> x | None -> 20) in
    let accounts = List.init per_page (fun _ -> random_account ()) in
    (* LATER ResStatus._200 *)
    (200, List.map (fun account -> (account_to_string, account)) accounts, [("Content-Type", "application/json")])
end

(*module Users = struct
  let meth = Method.GET
  let path = "/users/:userid"
  type params = {
    userid: string
  }
  type query = unit
  type body = string list
  type output = string
  let validate params query body session_data =
    true
  let handle params query body session_data =
    print_endline (match body with | h::r -> h | _ -> "lol") ;
    "hello"
end [@@deriving route]*)

(*

module Options = struct
  let mode = Mode.TCP
  let backend = Backend.Cohttp
  let port = 8000
  type session = {
    auth: { id: string ; token: string } option
  }
  let default_session = {
    auth = None
  }
end

module Server = Server.Make (Options)

module AuthMiddleware = struct
  let f params query body session_data =
    (params, query, body, Options.{ auth = Some { id = "56bd9" ; token = "1ac69123dda6" } })
end

let () =
  Server.register_middleware (module AuthMiddleware) ;
  Server.register_route (module Users) ;
  Server.listen ()

*)

let () =
  let accounts = List.init 10 (fun _ -> random_account ()) in
  List.iter (fun x -> x |> account_to_string |> Yojson.Safe.to_string |> print_endline) accounts


