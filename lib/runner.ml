open Core
open Async
open! Types

let exec ~prog ~argv ~stdout ~stderr () =
  let argv = List.concat [ [ "bash"; "./run_output.sh"; prog; stdout; stderr ]; argv ] in
  Log.Global.info_s [%message "run" ~prog:"bash" (argv : string list)];
  let%bind pid = Unix.fork_exec ~prog:"bash" ~argv () in
  let%bind exit_or_signal = Unix.waitpid pid in
  match exit_or_signal with
  | Ok () -> Deferred.Or_error.return ()
  | Error e ->
    let%bind stdout = Reader.with_file stdout ~f:Reader.contents in
    let%bind stderr = Reader.with_file stderr ~f:Reader.contents in
    (match e with
    | `Exit_non_zero code ->
      Deferred.Or_error.error_s [%message "Exit_non_zero" (code : int) stdout stderr]
    | `Signal signal ->
      Deferred.Or_error.error_s [%message "Signal" (signal : Signal.t) stdout stderr])
;;

let git_clone temp_dir repo_id branch_id =
  let run_id = Run_id.generate () in
  let repo = Repo_id.to_string repo_id in
  exec
    ~prog:"git"
    ~argv:
      [ "clone"
      ; [%string "git@github.com:%{repo}"]
      ; Filename.of_parts [ temp_dir; "repo" ]
      ; "-b"
      ; Branch_id.to_string branch_id
      ; "--depth"
      ; "1"
      ]
    ~stdout:[%string "logs/git-clone-%{run_id#Run_id}.out.log"]
    ~stderr:[%string "logs/git-clone-%{run_id#Run_id}.err.log"]
    ()
  >>| Or_error.ok_exn
;;

let list_tasks =
  Lazy.from_fun (fun () ->
      Sys.ls_dir "tasks"
      >>| List.filter ~f:(fun branch -> not String.(branch = ".git"))
      >>= Deferred.List.map ~f:(fun branch ->
              let branch_id = Branch_id.of_string branch in
              let%bind tasks =
                Sys.ls_dir (Filename.of_parts [ "tasks"; branch ])
                >>| List.map ~f:Task_id.of_string
              in
              return (branch_id, tasks))
      >>| Branch_id.Map.of_alist_exn)
;;

let list_branch_tasks branch_id =
  let%map tasks = Lazy.force list_tasks in
  Branch_id.Map.find_exn tasks branch_id
;;

let run_task temp_dir _repo_id branch_id task_id =
  let run_id = Run_id.generate () in
  let%bind result =
    let open Deferred.Or_error.Let_syntax in
    let solution_filename =
      Filename.of_parts
        [ temp_dir; "repo"; Branch_id.to_string branch_id; Task_id.to_string task_id ]
    in
    let test_filename =
      Filename.of_parts
        [ "tasks"; Branch_id.to_string branch_id; Task_id.to_string task_id ]
    in
    let main_filename = Filename.of_parts [ temp_dir; "main.hs" ] in
    let executable_filename = Filename.of_parts [ temp_dir; "main.exe" ] in
    let%bind () =
      Deferred.Or_error.of_exn_result (Unix.access solution_filename [ `Read ])
    in
    let%bind () =
      exec
        ~prog:"bash"
        ~argv:[ "./make_runnable.sh"; test_filename; solution_filename; main_filename ]
        ~stdout:[%string "logs/make_runnable-%{run_id#Run_id}.out.log"]
        ~stderr:[%string "logs/make_runnable-%{run_id#Run_id}.err.log"]
        ()
    in
    let%bind () =
      exec
        ~prog:"ghc"
        ~argv:[ main_filename; "-O2"; "-o"; executable_filename ]
        ~stdout:[%string "logs/ghc-%{run_id#Run_id}.out.log"]
        ~stderr:[%string "logs/ghc-%{run_id#Run_id}.err.log"]
        ()
    in
    let%bind () =
      exec
        ~prog:"bash"
        ~argv:[ "./run_limits.sh"; executable_filename ]
        ~stdout:[%string "logs/run_limits-%{run_id#Run_id}.out.log"]
        ~stderr:[%string "logs/run_limits-%{run_id#Run_id}.err.log"]
        ()
    in
    return ()
  in
  Deferred.return (task_id, run_id, result)
;;

let rec remove_recursive path =
  match%bind Sys.is_directory path with
  | `Yes ->
    let%bind subdirs = Sys.ls_dir path in
    let%bind () =
      Deferred.List.iter subdirs ~f:(fun subdir ->
          remove_recursive (Filename.of_parts [ path; subdir ]))
    in
    Unix.rmdir path
  | `No -> Unix.remove path
  | `Unknown -> (* ¯\_(ツ)_/¯ *) Deferred.return ()
;;

let run repo_id branch_id =
  let%bind temp_dir = Unix.mkdtemp "hse-spb-2021-fp" in
  let%bind () = git_clone temp_dir repo_id branch_id in
  let%bind tasks = list_branch_tasks branch_id in
  let%bind ret = Deferred.List.map tasks ~f:(run_task temp_dir repo_id branch_id) in
  let%bind () = remove_recursive temp_dir in
  return ret
;;
