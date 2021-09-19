open Core
open Async
open Cohttp
open Cohttp_async
open Types
module Run = Run

module Github_creds = struct
  type t = { iss : int } [@@deriving sexp]

  let to_auth_string { iss } =
    let%bind key =
      Reader.with_file "hse-spb-2021-fp.2021-09-18.private-key.pem" ~f:(fun reader ->
          let%bind s = Reader.contents reader in
          let cst = Cstruct.of_string s in
          Deferred.return (Option.value_exn (Result.ok (X509.Private_key.decode_pem cst))))
    in
    let key =
      match key with
      | `RSA key -> key
      | _ -> raise_s [%message "unknown key type"]
    in
    let jwk = Jose.Jwk.make_priv_rsa key in
    let header = Jose.Header.make_header jwk in
    let iat =
      Int.of_float (Time.Span.to_sec (Time.to_span_since_epoch (Time.now ()))) - 60
    in
    let exp = iat + 600 in
    let payload =
      Jose.Jwt.empty_payload
      |> Jose.Jwt.add_claim "iat" (`Int iat)
      |> Jose.Jwt.add_claim "exp" (`Int exp)
      |> Jose.Jwt.add_claim "iss" (`Int iss)
    in
    let jwt =
      match Jose.Jwt.sign ~header ~payload jwk with
      | Ok jwt -> jwt
      | Error (`Msg e) -> raise_s [%message e]
    in
    return [%string "Bearer %{jwt#Jose.Jwt}"]
  ;;
end

let post_comment repo pr contents =
  let%bind creds =
    Reader.load_sexp "creds.sexp" Github_creds.t_of_sexp >>| Or_error.ok_exn
  in
  let%bind auth_string = Github_creds.to_auth_string creds in
  Log.Global.info_s [%message auth_string];
  let endpoint =
    [%string
      "https://api.github.com/repos/%{repo#Repo_id}/issues/%{pr#Pull_request_id}/comments"]
  in
  let headers = Header.init () in
  let headers = Header.add headers "Accept" "application/vnd.github.v3+json" in
  let headers = Header.add headers "Authorization" auth_string in
  let body = Body.of_string [%string "{\"body\":\"%{contents}\"}"] in
  Log.Global.info_s [%message endpoint (headers : Header.t) (body : Body.t)];
  let%bind resp, _resp_body = Client.post ~headers ~body (Uri.of_string endpoint) in
  let status = Response.status resp in
  Log.Global.info_s [%message "response" (status : Code.status_code)];
  Deferred.unit
;;

let process_hook sink body _req =
  let%bind body = Body.to_string body in
  let%bind () =
    match Event.of_string body with
    | Ok (Pull_request pr) ->
      Log.Global.info_s [%message "Pull request" (pr : Event.Pull_request.t)];
      let%bind () =
        post_comment
          pr.Event.Pull_request.repo
          pr.Event.Pull_request.pull_request
          "I see your PR!"
      in
      Pipe.write sink (pr.Event.Pull_request.repo, pr.Event.Pull_request.branch)
    | Error e ->
      Log.Global.error_s [%message "Error" (e : Error.t)];
      Deferred.unit
  in
  Server.respond_string "<h1>Hello from fp.vasalf.net</h1>"
;;

let status pg_conn repo branch task =
  let result = ref None in
  let%bind () =
    Postgres_async.query
      pg_conn
      [%string
        "SELECT result FROM fp_results WHERE repo = '%{repo#Repo_id}' AND branch = \
         '%{branch#Branch_id}' AND task = '%{task#Task_id}' ORDER BY id DESC LIMIT 1;"]
      ~handle_row:(fun ~column_names:_ ~values -> result := Array.get values 0)
    >>| Or_error.ok_exn
  in
  return !result
;;

let status_table pg_conn repo =
  let%bind tasks = Lazy.force Runner.list_tasks in
  let task_row branch task =
    let%bind result = status pg_conn repo branch task in
    let result = Sexp.to_string (Option.sexp_of_t String.sexp_of_t result) in
    return
      [%string
        "<tr><td>%{branch#Branch_id}</td><td>%{task#Task_id}</td><td>%{result}</td></tr>"]
  in
  let header = [%string "<h2>%{repo#Repo_id}</h2>"] in
  let thead = "<tr><th>HW</th><th>Task</th><th>Result</th></tr>" in
  let%bind rows =
    tasks
    |> Map.to_alist
    |> List.map ~f:(fun (branch, tasks) -> List.map tasks ~f:(fun task -> branch, task))
    |> List.concat
    |> Deferred.List.map ~f:(fun (branch, task) -> task_row branch task)
    >>| String.concat
  in
  return
    [%string
      "%{header}<table \
       class=\"table\"><thead>%{thead}</thead><tbody>%{rows}</tbody></table>"]
;;

let seen_repos pg_conn =
  let result = ref [] in
  let%bind () =
    Postgres_async.query
      pg_conn
      "SELECT DISTINCT repo FROM fp_results ORDER BY repo;"
      ~handle_row:(fun ~column_names:_ ~values ->
        result := Repo_id.of_string (Option.value_exn (Array.get values 0)) :: !result)
    >>| Or_error.ok_exn
  in
  return !result
;;

let pack s =
  let bootstrap =
    "<link \
     href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css\" \
     rel=\"stylesheet\" \
     integrity=\"sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU\" \
     crossorigin=\"anonymous\">"
  in
  let head = [%string "<title>fp.vasalf.net</title>%{bootstrap}</head>"] in
  let body = [%string "<div class=\"container\">%{s}</div>"] in
  [%string "<!doctype html5><html>%{head}<head><body>%{body}</body></html>"]
;;

let process_home pg_conn =
  seen_repos pg_conn
  >>= Deferred.List.map ~f:(status_table pg_conn)
  >>| String.concat
  >>| pack
  >>= Server.respond_string
;;

let server =
  let%map_open.Command () = return () in
  fun () ->
    Mirage_crypto_rng_unix.initialize ();
    let%bind pg_conn =
      let%bind { Postgres_creds.username; password; database } =
        Reader.with_file "pgcreds.sexp" ~f:(fun reader ->
            Reader.contents reader >>| Sexp.of_string >>| Postgres_creds.t_of_sexp)
      in
      Postgres_async.connect ~user:username ~password ~database () >>| Or_error.ok_exn
    in
    let sink_reader, sink_writer = Pipe.create () in
    let run_job =
      Pipe.iter sink_reader ~f:(fun (repo, branch) ->
          Run.run_and_report pg_conn repo branch)
    in
    ignore run_job;
    let callback ~body addr req =
      let uri = Request.uri req in
      let meth = Request.meth req in
      Log.Global.info_s
        [%message (addr : Socket.Address.Inet.t) (Uri.to_string uri) (meth : Code.meth)];
      match Uri.path uri with
      | "/hook" -> process_hook sink_writer body req
      | "/" -> process_home pg_conn
      | _ -> Server.respond_string "404"
    in
    let where_to_listen = Tcp.Where_to_listen.of_port 8179 in
    let%bind server = Server.create ~on_handler_error:`Raise where_to_listen callback in
    ignore server;
    Deferred.never ()
;;

let command = Command.async ~summary:"Run the bot" server
