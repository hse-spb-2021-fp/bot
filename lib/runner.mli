open Core
open Async
open Types

val list_tasks : Task_id.t list Branch_id.Map.t Deferred.t Lazy.t

val run
  :  Repo_id.t
  -> Branch_id.t
  -> (Task_id.t * Run_id.t * unit Or_error.t) list Deferred.t
