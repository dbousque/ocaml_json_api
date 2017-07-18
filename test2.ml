

module Users = struct
  let meth = Method.GET
  let path = "/users/:userid"
  type params = {
    userid: string
  } [@@deriving yojson]
  type query = unit [@@deriving yojson]
  type body = string list [@@deriving yojson]
  type output = string [@@deriving yojson]
  let validate params query body session_data =
    true
  let handle params query body session_data =
    print_endline (match body with | h::r -> h | _ -> "lol") ;
    "hello"
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

module Options = struct
  let mode = Mode.TCP
  let backend = Backend.Cohttp
  let port = 8000
  type session_data = {
    auth_id: string ;
    auth_token: string
  }
  let default_session_data = {
    auth_id = "" ;
    auth_token = ""
  }
end

module Server = Server.Make (Options)

module AuthMiddleware = struct
  let f params query body session_data =
    (params, query, body, Options.{ auth_id = "56bd9" ; auth_token = "1ac69123dda6" })
end

let () =
  Server.register_middleware (module AuthMiddleware) ;
  Server.register_route (module Users) ;
  Server.listen ()