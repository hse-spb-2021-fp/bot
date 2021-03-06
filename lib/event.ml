open Core
open Types

module Pull_request = struct
  type t =
    { repo : Repo_id.t
    ; branch : Branch_id.t
    ; pull_request : Pull_request_id.t
    ; name : string
    }
  [@@deriving sexp]
end

type t = Pull_request of Pull_request.t [@@deriving sexp]

let of_string s =
  let open Yojson.Safe in
  Or_error.try_with (fun () ->
      let json = from_string s in
      let repo =
        json
        |> Util.member "repository"
        |> Util.member "full_name"
        |> Util.to_string
        |> Repo_id.of_string
      in
      let branch =
        json
        |> Util.member "pull_request"
        |> Util.member "head"
        |> Util.member "ref"
        |> Util.to_string
        |> Branch_id.of_string
      in
      let pull_request =
        json
        |> Util.member "pull_request"
        |> Util.member "number"
        |> Util.to_int
        |> Int.to_string
        |> Pull_request_id.of_string
      in
      let name =
        json |> Util.member "pull_request" |> Util.member "title" |> Util.to_string
      in
      Pull_request { Pull_request.repo; branch; pull_request; name })
;;
