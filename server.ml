

open Lwt
open Cohttp

type 'a result = Error of string | Ok of 'a

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
    type query
    type body
    type output
    val validate : params -> query -> body -> session_data_type -> bool
    val handle : params -> query -> body -> session_data_type -> output
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
      type params  (* ? (+ query) -> maybe params ('/users/:user_id/show') and query ('?key=value') *)
      type query
      type body
      type output
      val validate : params -> query -> body -> session_data_type -> bool
      val handle : params -> query -> body -> session_data_type -> output
    end

    let middlewares = ref []
    let routes = ref []

    let register_middleware (module Middleware : MIDDLEWARE) =
      middlewares := (module Middleware : MIDDLEWARE) :: !middlewares

    let register_route (module Route : ROUTE) =
      routes := (module Route : ROUTE) :: !routes

    let find_route uri_str meth_str =
      let uri = Uri.of_string uri_str in
      let matching_uri (module Route : ROUTE) =
        let route_path = Parse.path_elements Route.path in
        let ori_path = uri |> Uri.path |> Parse.path_elements in
        if List.length route_path <> List.length ori_path then false
        else (
          let merged = Utils.list_merge route_path ori_path in
          let matching_path_element (route_e, ori_e) =
            route_e = ori_e || String.sub route_e 0 1 = ":"
          in
          let filtered = List.filter matching_path_element merged in
          List.length merged = List.length filtered
        )
      in
      match Method.from_string meth_str with
      | None -> Error ("Unknown method \"" ^ meth_str ^ "\"")
      | Some meth -> (
        let candidates = List.filter (fun (module Route : ROUTE) -> Route.meth = meth) !routes in
        let candidates = List.filter matching_uri candidates in
        match candidates with
        | h :: [] -> Ok h
        | h :: tl -> Error ("Multiple routes matching the uri \"" ^ uri_str ^ "\"")
        | [] -> Error ("No route matching the uri \"" ^ uri_str ^ "\"")
      )

    let make_response uri meth headers body =
      match find_route uri meth with
      | Error err -> (Lwt_io.printf "ERROR : %s\n" err ; ("error", 500))
      | Ok (module Route : ROUTE) -> (
        Lwt_io.printf "found route\n" ;
        let query_yojson = Parse.uri_query uri in
        let params = () in
        let body = body in
        let session_data = Options.default_session_data in
        let call_middleware (params, query, body, session_data) (module Middleware : MIDDLEWARE) =
          Middleware.f params query body session_data
        in
        let (params, query, body, session_data) = List.fold_left call_middleware middlewares in
        match Route.validate params query body session_data with
        | false -> ("error, route didn't validate", 500)
        | true -> (
          let output = Route.handle params query body session_data in
          let res = output |> output_to_yojson |> Yojson.Safe.to_string in
          res, 200
        )
        print_endline (Yojson.Safe.to_string params_yojson) ;
        "ok", 200
      )

    let listen () =
      List.iter (fun (module Route : ROUTE) -> ignore (Lwt_io.printf "path : %s\n" Route.path)) !routes ;
      let server =
        let callback _conn req body =
          let uri = req |> Request.uri |> Uri.to_string in
          let meth = req |> Request.meth |> Code.string_of_method in
          let headers = req |> Request.headers |> Header.to_string in
          body |> Cohttp_lwt_body.to_string >|=
            (fun body -> make_response uri meth headers body)
              >>= (fun (body, status) ->
                let status = Cohttp.Code.status_of_code status in
                Cohttp_lwt_unix.Server.respond_string ~status ~body ()
              )
        in
        let mode = match mode with
          | Mode.TCP -> `TCP (`Port port)
        in
        Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) 
      in
      ignore (Lwt_main.run server)

  end

