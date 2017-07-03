

(*open Lwt
open Cohttp
open Cohttp_lwt_unix

 let server =
  let callback _conn req body =
    body |> Cohttp_lwt_body.to_string >|= (fun body -> )
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())  *)

module Users = struct
  let meth = Method.GET
  let path = "/users"
  type params = int [@@deriving yojson]
  type body = string list [@@deriving yojson]
  type output = string [@@deriving yojson]
  let validate params body =
    true
  let handle params body session_data =
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

module Server = Json_server.Make (Options)

module AuthMiddleware = struct
  let f body params session_data =
    (body, params, Options.{ auth_id = "56bd9" ; auth_token = "1ac69123dda6" })
end

let () =
  Server.register_middleware (module AuthMiddleware) ;
  Server.register_route (module Users) ;
  Server.listen ()