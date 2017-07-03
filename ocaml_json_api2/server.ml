

type mode =
  | TCP
  | UDP

module type SERVEROPTIONS =
sig
  val mode : mode
  val port : int
  val backend : Backend.t
  type session_data
end

module type SERVER =
sig
  val mode : mode
  val port : port
  val backend : Backend.t
  val middlewares : 
  type session_data

  module type MIDDLEWARE =
  sig
    let f : Yojson.Safe.t -> Yojson.Safe.t -> session_data ->
                      (Yojson.Safe.t * Yojson.Safe.t * session_data)
  end

  val register_middleware : (module Middleware : MIDDLEWARE) -> ()
end

module type MAKESERVER =
  functor (Options : SERVEROPTIONS) ->
    SERVER

module Make : MAKESERVER =
  functor (Options : SERVEROPTIONS) ->
  struct
    let host = Connection.host
    let port = Connection.port
    let db = Connection.db
    let ok = true
  end