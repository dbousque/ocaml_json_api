

module type ROUTE =
sig
  val method : string
  val path : string
  type params
  type body
  type output
  val validate : params -> body -> bool
end

module type MAKEROUTE =
  functor (Route : ROUTE) ->
    DB

module Make : MAKEROUTE =
  functor (Route : ROUTE) ->
  struct
    let host = Connection.host
    let port = Connection.port
    let db = Connection.db
    let ok = true
  end