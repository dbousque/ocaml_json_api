

open Lwt
open Cohttp

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

  module type ROUTE =
  sig
    val meth : Method.t
    val path : string
    type params
    type body
    type output
    val validate : params -> body -> bool
    val handle : params -> body -> session_data_type -> output
  end

  val mode : Mode.t
  val port : int
  val backend : Backend.t

  val middlewares : (module MIDDLEWARE) list ref
  val routes : (module ROUTE) list ref

  val register_middleware : (module MIDDLEWARE) -> unit
  val register_route : (module ROUTE) -> unit
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

    module type ROUTE =
    sig
      val meth : Method.t
      val path : string
      type params ? (+ query) -> maybe params ('/users/:user_id/show') and query ('?key=value')
      type body
      type output
      val validate : params -> body -> bool
      val handle : params -> body -> session_data_type -> output
    end

    let middlewares = ref []
    let routes = ref []

    let register_middleware (module Middleware : MIDDLEWARE) =
      middlewares := (module Middleware : MIDDLEWARE) :: !middlewares

    let register_route (module Route : ROUTE) =
      routes := (module Route : ROUTE) :: !routes

    let make_response uri meth headers body =
      let params_yojson = Parse.uri_params uri in
      print_endline (Yojson.Safe.to_string params_yojson) ;
      body, `OK

    let listen () =
      List.iter (fun (module Route : ROUTE) -> ignore (Lwt_io.printf "path : %s\n" Route.path)) !routes ;
      let server =
        let callback _conn req body =
          let uri = req |> Request.uri |> Uri.to_string in
          let meth = req |> Request.meth |> Code.string_of_method in
          let headers = req |> Request.headers |> Header.to_string in
          body |> Cohttp_lwt_body.to_string >|=
            (fun body -> make_response uri meth headers body)
              >>= (fun (body, status) -> Cohttp_lwt_unix.Server.respond_string ~status ~body ())
        in
        let mode = match mode with
          | Mode.TCP -> `TCP (`Port port)
        in
        Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) 
      in
      ignore (Lwt_main.run server)

  end

