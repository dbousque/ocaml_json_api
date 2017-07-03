

open Lwt
open Cohttp
open Cohttp_lwt_unix

type inptype = {
  integer: int ;
  chaine: string
} [@@deriving yojson]

let parse_body body =
  match (body |> Yojson.Safe.from_string |> inptype_of_yojson) with
  | Result.Ok b -> b
  | Result.Error e -> failwith e

let serialize_body body =
  body |> inptype_to_yojson |> Yojson.Safe.to_string

let server =
  let callback _conn req body =
    let inp = inptype_to_yojson ({ integer = 14 ; chaine = "bonjour" }) in
    let inp_str = Yojson.Safe.to_string inp in
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s\nRes: %s"
         uri meth headers body (serialize_body (parse_body body))))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8002)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)