

module ContentType = struct
  type t = Json | Html | Other of string
end

module GetUserAccounts = struct
  [%disko GET]
  let path = "/users/<userid>/accounts"

  type params = {
    userid: int [@validate userid >= 0]
  }

  type query = {
    page: int [@default 1] [@validate page > 0 ; "page can't be <= 0"] ;
    per_page: int option [@validate per_page >= 5 && per_page <= 20 ; "per_page must be between 5 and 20"] ;
    filter: string [@when_missing "Missing filter"]
  }

  type body = string list

  (*

  type output = account list    CANT USE DERIVING YOSJON AUTOMATICALLY, since account is defined somewhere else, so let people define their output_to_string functions, or let them return strings

  let output_content_type = ContentType.Json

  let output_to_string out =
    List.map account_to_string out

  *)
end

(*
module GetUserAccounts = struct
  let method = "GET"
  let path = "/users/<userid>/accounts"

  let _disko_validate_params_userid userid =
    match userid >= 0 with
    | true -> None
    | false -> Some "Invalid \"userid\" /users/<userid>/accounts"

  let _disko_parse_params sent_path =
    let userid = Regex.parse "/users/([.^/]*)/accounts" in
    let userid = try int_of_string userid with | Conversion_failed -> "Expected \"userid\" to be a valid int /users/<userid>/accounts" in
    _disko_validate_params_userid userid
    { userid = userid }

  type params = {
    userid: int
  }

  let _disko_validate_query_page page =
    match page >= 0 with
    | true -> None
    | false -> Some "page can't be < 0"

  let _disko_parse_query sent_query =
    let page
    let page = try int_of_string sent_query['page'] with | Conversion_failed -> "Expected query param \"page\" to be a valid int" in


  type query = {
    page: int ;
    per_page: int option ;
    filter: string
  }

  type body = string list
end
*)