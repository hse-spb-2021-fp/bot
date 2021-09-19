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

module Run_id = struct
  include
    String_id.Make
      (struct
        let module_name = "Run_id"
      end)
      ()

  let generate () = of_string (Uuid.to_string (Uuid_unix.create ()))
end

module Task_id =
  String_id.Make
    (struct
      let module_name = "Task_id"
    end)
    ()

module Postgres_creds = struct
  type t =
    { username : string
    ; password : string
    ; database : string
    }
  [@@deriving sexp]
end
