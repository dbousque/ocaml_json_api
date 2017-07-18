

type t =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

let from_string = function
  | "get" -> Some GET
  | "Get" -> Some GET
  | "GET" -> Some GET
  | "post" -> Some POST
  | "Post" -> Some POST
  | "POST" -> Some POST
  | "put" -> Some PUT
  | "Put" -> Some PUT
  | "PUT" -> Some PUT
  | "patch" -> Some PATCH
  | "Patch" -> Some PATCH
  | "PATCH" -> Some PATCH
  | "delete" -> Some DELETE
  | "Delete" -> Some DELETE
  | "DELETE" -> Some DELETE
  | _ -> None