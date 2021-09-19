open Core
open Async
open Types
module Run_id : String_id.S
module Task_id : String_id.S

val list_tasks : Task_id.t list Branch_id.Map.t Deferred.t Lazy.t
val run : Repo_id.t -> Branch_id.t -> (Run_id.t * unit Or_error.t) list Deferred.t
val command : Command.t
