open Core
module Repo_id : String_id.S
module Branch_id : String_id.S
module Pull_request_id : String_id.S

module Run_id : sig
  include String_id.S

  val generate : unit -> t
end

module Task_id : String_id.S

module Postgres_creds : sig
  type t =
    { username : string
    ; password : string
    ; database : string
    }
  [@@deriving sexp]
end
