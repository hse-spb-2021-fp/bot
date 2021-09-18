open Core

module Repo_id =
  String_id.Make
    (struct
      let module_name = "Repo_id"
    end)
    ()

module Branch_id =
  String_id.Make
    (struct
      let module_name = "Branch_id"
    end)
    ()

module Pull_request_id =
  String_id.Make
    (struct
      let module_name = "Pull_request_id"
    end)
    ()
