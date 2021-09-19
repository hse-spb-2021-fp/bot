open Core
open Async
open Types

let report pg_conn repo_id branch_id results =
  Deferred.List.iter results ~f:(fun (task_id, run_id, result) ->
      let result =
        match result with
        | Ok () -> "OK"
        | Error _ -> "ERROR"
      in
      Postgres_async.query_expect_no_data
        pg_conn
        [%string
          "INSERT INTO fp_results (repo, branch, task, run_id, result) VALUES \
           ('%{repo_id#Repo_id}','%{branch_id#Branch_id}', '%{task_id#Task_id}', \
           '%{run_id#Run_id}', '%{result}');"]
      >>| Or_error.ok_exn)
;;

let run_and_report pg_conn repo_id branch_id =
  let%bind results = Runner.run repo_id branch_id in
  report pg_conn repo_id branch_id results
;;

let main =
  let%map_open.Command () = return ()
  and repo = flag "-repo" (required string) ~doc:"repository name"
  and branch = flag "-branch" (required string) ~doc:"branch name" in
  fun () ->
    let repo_id = Repo_id.of_string repo in
    let branch_id = Branch_id.of_string branch in
    let%bind runs = Runner.run repo_id branch_id in
    print_s [%message (runs : (Task_id.t * Run_id.t * unit Or_error.t) list)];
    let%bind { Postgres_creds.username; password; database } =
      Reader.with_file "pgcreds.sexp" ~f:(fun reader ->
          Reader.contents reader >>| Sexp.of_string >>| Postgres_creds.t_of_sexp)
    in
    let%bind pg_conn =
      Postgres_async.connect ~user:username ~password ~database () >>| Or_error.ok_exn
    in
    report pg_conn repo_id branch_id runs
;;

let command = Command.async ~summary:"Run an invocation" main
