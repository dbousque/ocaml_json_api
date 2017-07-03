

module type ROUTE =
sig
  val meth : Method.t
  val path : string
  type params
  type body
  type output
  val validate : params -> body -> bool
end