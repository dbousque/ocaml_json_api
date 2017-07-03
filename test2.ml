

open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    body |> Cohttp_lwt_body.to_string >|= (fun body -> )
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

module Users = struct
  let method = Method.GET
  let path = "/users"
  type params = int [@@deriving yojson]
  type body = string list [@@deriving yojson]
  type output = string [@@deriving yojson]
  let validate params body =
    true
  let handle params body session_data =
    "hello"
end

module AuthMiddleware = struct
  let f body params session_data =
    (body, params, { authId = "56bd9" ; authToken = "1ac69123dda6" })
end

let () =
  module Options = struct
    let mode = `TCP
    let port = 8000
    let backend = Backend.Cohttp
    type session_data = {
      authId: string ;
      authToken: string
    }
  end
  module Server = Server.Make (Options)
  Server.register_middleware (module AuthMiddleware)
  Server.register_route (module Users)
  Server.listen ()
  ignore (Lwt_main.run server)