

open Lwt
open Cohttp
open Cohttp_lwt_unix

module type SERVEROPTIONS =
sig
  val mode : Mode.t
  val port : int
  val backend : Backend.t
  type session_data
end

module DefaultOptions : SERVEROPTIONS =
struct
  let mode = Mode.TCP
  let port = 8080
  let backend = Backend.Cohttp
  type session_data = unit
end

module type SERVER =
sig
  type session_data_type

  module type MIDDLEWARE =
  sig
    val f : Yojson.Safe.json -> Yojson.Safe.json -> session_data_type ->
                  (Yojson.Safe.json * Yojson.Safe.json * session_data_type)
  end

  val mode : Mode.t
  val port : int
  val backend : Backend.t

  val middlewares : (module MIDDLEWARE) list ref
  val routes : (module Route.ROUTE) list ref

  val register_middleware : (module MIDDLEWARE) -> unit
  val register_route : (module Route.ROUTE) -> unit
  val listen : unit -> unit
end

module type MAKESERVER =
  functor (Options : SERVEROPTIONS) ->
    SERVER with type session_data_type = Options.session_data

module Make : MAKESERVER =
  functor (Options : SERVEROPTIONS) ->
  struct
    include Options

    type session_data_type = Options.session_data

    module type MIDDLEWARE =
    sig
      val f : Yojson.Safe.json -> Yojson.Safe.json -> session_data_type ->
                    (Yojson.Safe.json * Yojson.Safe.json * session_data_type)
    end

    let middlewares = ref []
    let routes = ref []

    let register_middleware (module Middleware : MIDDLEWARE) =
      middlewares := (module Middleware : MIDDLEWARE) :: !middlewares

    let register_route (module Rout : Route.ROUTE) =
      routes := (module Rout : Route.ROUTE) :: !routes

    let listen () =
      List.iter (fun (module Rout : Route.ROUTE) -> Printf.printf "path : %s\n" Rout.path) !routes ;
      Printf.printf "Listening on port %d\n" port ;
      let server =
        let callback _conn req body =
          body |> Cohttp_lwt_body.to_string >|= (fun body -> body)
          >>= (fun body -> Server.respond_string ~status:`OK ~body ())
        in
        let mode = match mode with
          | Mode.TCP -> `TCP (`Port port)
        in
        Server.create ~mode (Server.make ~callback ()) 
      in
      ignore (Lwt_main.run server)

  end

