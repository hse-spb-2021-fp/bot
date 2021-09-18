open! Core
open Types

module Pull_request : sig
  type t =
    { repo : Repo_id.t
    ; branch : Branch_id.t
    ; pull_request : Pull_request_id.t
    ; name : string
    }
  [@@deriving sexp]
end

type t = Pull_request of Pull_request.t [@@deriving sexp]

val of_string : string -> t Or_error.t
