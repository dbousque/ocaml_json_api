

module Users = struct
  let meth = Method.GET
  let path = "/users"
  type params = int [@@deriving yojson]
  type body = string list [@@deriving yojson]
  type output = string [@@deriving yojson]
  let validate params body =
    true
  let handle params body session_data =
    print_endline (match body with | h::r -> h | _ -> "lol") ;
    "hello"
end

module Options = struct
  let mode = Mode.TCP
  let backend = Backend.Cohttp
  let port = 8000
  type session_data = {
    auth_id: string ;
    auth_token: string
  }
end

module Server = Server.Make (Options)

module AuthMiddleware = struct
  let f body params session_data =
    (body, params, Options.{ auth_id = "56bd9" ; auth_token = "1ac69123dda6" })
end

let () =
  Server.register_middleware (module AuthMiddleware) ;
  Server.register_route (module Users) ;
  Server.listen ()