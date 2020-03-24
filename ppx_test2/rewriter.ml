open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(*

let valid_http_methods = ["GET" ; "POST" ; "PUT" ; "PATCH" ; "DELETE" ; "OPTIONS" ; "HEAD" ; "CONNECT" ; "TRACE"]
let valid_http_methods_string = List.fold_left (fun acc m -> acc ^ "|" ^ m) "" valid_http_methods
let valid_http_methods_string = String.sub valid_http_methods_string 1 ((String.length valid_http_methods_string) - 1)

let expr_mapper mapper expr = 
  match expr with
  | { pexp_desc = Pexp_extension ({ txt = "route"; loc }, pstr) } -> (
    raise (Location.Error (Location.error ~loc ("Invalid \"route\" usage: should be used as the first expression in a module [%route " ^ valid_http_methods_string ^ "]")))
  )
  | x -> default_mapper.expr mapper x

let module_expr_mapper mapper module_expr =
  match module_expr with
  | { pmod_desc = Pmod_structure ({ pstr_desc = Pstr_eval ( { pexp_desc = Pexp_extension ({ txt = "route"; loc }, pstr) }, _ ) } :: rest) } -> (
    match pstr with
    | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct ({ txt }, None) }, _) }] -> (
      match txt with
      | Lident str -> (
        let valid_method = List.exists (fun m -> m = str) valid_http_methods in
        match valid_method with
        | false -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" usage: method \"" ^ str ^ "\" not in " ^ valid_http_methods_string)))
        | true -> (
          { module_expr with pmod_desc = Pmod_structure rest }
        )
      )
      | _ -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" syntax: expected [%route " ^ valid_http_methods_string ^ "]")))
    )
    | _ -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" syntax: expected [%route " ^ valid_http_methods_string ^ "]")))
  )
  | x -> default_mapper.module_expr mapper x

*)

let valid_http_methods = ["GET" ; "POST" ; "PUT" ; "PATCH" ; "DELETE" ; "OPTIONS" ; "HEAD" ; "CONNECT" ; "TRACE"]
let valid_http_methods_string = List.fold_left (fun acc m -> acc ^ "|" ^ m) "" valid_http_methods
let valid_http_methods_string = String.sub valid_http_methods_string 1 ((String.length valid_http_methods_string) - 1)

let expr_mapper mapper expr = 
  match expr with
  | { pexp_desc = Pexp_extension ({ txt = "route"; loc }, pstr) } -> (
    raise (Location.Error (Location.error ~loc "Invalid \"[%disko route]\" usage: should be used as the first expression in a module"))
  )
  | x -> default_mapper.expr mapper x

let ensure_module_elts_exist loc module_elts names =
  let elt_names_kinds = List.map (fun module_elt -> (
    match module_elt with
    | { pstr_desc = Pstr_value (_, [{ pvb_pat = { ppat_desc = Ppat_var { txt } } }] ) } -> Some (txt, "value")
    | { pstr_desc = Pstr_type (_, [{ ptype_name = { txt } }] ) } -> Some (txt, "type")
    | _ -> None
  )) module_elts in
  List.iter ((fun (name, kind) -> 
    let elt = List.find_opt (fun elt -> match elt with | Some (elt_name, _) -> elt_name = name | None -> false) elt_names_kinds in
    match elt with
    | Some (Some (_, elt_kind)) -> (
      match elt_kind = kind with
      | true -> ()
      | false -> raise (Location.Error (Location.error ~loc ("Invalid \"[%route]\": expected \"" ^ name ^ "\" to be a " ^ kind ^ " but it is a " ^ elt_kind)))
    )
    | _ -> raise (Location.Error (Location.error ~loc ("Invalid \"[%route]\": missing \"" ^ name ^ "\" " ^ kind ^ " in module")))
  )) names

let handle_route_module loc module_elts meth =
  ensure_module_elts_exist loc module_elts [("path", "value") ; ("query", "type") ; ("body", "type") ; ("output", "type") ] ;
  module_elts

let module_expr_mapper mapper module_expr =
  match module_expr with
  | { pmod_desc = Pmod_structure ({ pstr_desc = Pstr_eval ( { pexp_desc = Pexp_extension ({ txt = "route"; loc }, pstr) }, _ ) } :: rest) } -> (
    match pstr with
    | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct ({ txt }, None) }, _) }] -> (
      match txt with
      | Lident str -> (
        let valid_method = List.exists (fun m -> m = str) valid_http_methods in
        match valid_method with
        | false -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" usage: method \"" ^ str ^ "\" not in " ^ valid_http_methods_string)))
        | true -> (
          { module_expr with pmod_desc = Pmod_structure (handle_route_module loc rest str) }
        )
      )
      | _ -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" syntax: expected [%route " ^ valid_http_methods_string ^ "]")))
    )
    | _ -> raise (Location.Error (Location.error ~loc ("Invalid \"route\" syntax: expected [%route " ^ valid_http_methods_string ^ "]")))
  )
  | x -> default_mapper.module_expr mapper x

let route_mapper argv =
  { 
    default_mapper with
    expr = expr_mapper ;
    module_expr = module_expr_mapper
  }
 
let () = register "route" route_mapper