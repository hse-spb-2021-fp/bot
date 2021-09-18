open Core
open Async
open Cohttp
open Cohttp_async
open Types

let post_comment repo pr contents =
  let endpoint =
    [%string
      "https://api.github.com/repos/%{repo#Repo_id}/issues/%{pr#Pull_request_id}/comments"]
  in
  let headers = Header.add (Header.init ()) "Accept" "application/vnd.github.v3+json" in
  let body = Body.of_string [%string "{\"body\":\"%{contents}\"}"] in
  let%bind resp, _resp_body = Client.post ~headers ~body (Uri.of_string endpoint) in
  let status = Response.status resp in
  Log.Global.info_s [%message "response" (status : Code.status_code)];
  Deferred.unit
;;

let process_hook body _req =
  let%bind body = Body.to_string body in
  let%bind () =
    match Event.of_string body with
    | Ok (Pull_request pr) ->
      Log.Global.info_s [%message "Pull request" (pr : Event.Pull_request.t)];
      post_comment
        pr.Event.Pull_request.repo
        pr.Event.Pull_request.pull_request
        "I see your PR!"
    | Error e ->
      Log.Global.error_s [%message "Error" (e : Error.t)];
      Deferred.unit
  in
  Server.respond_string "<h1>Hello from fp.vasalf.net</h1>"
;;

let server =
  let%map_open.Command () = return () in
  fun () ->
    let callback ~body addr req =
      let uri = Request.uri req in
      let meth = Request.meth req in
      Log.Global.info_s
        [%message (addr : Socket.Address.Inet.t) (Uri.to_string uri) (meth : Code.meth)];
      match Uri.path uri with
      | "/hook" -> process_hook body req
      | _ -> Server.respond_string "<h1>Hello from fp.vasalf.net</h1>"
    in
    let where_to_listen = Tcp.Where_to_listen.of_port 8179 in
    let%bind server = Server.create ~on_handler_error:`Raise where_to_listen callback in
    ignore server;
    Deferred.never ()
;;

let command = Command.async ~summary:"Run the bot" server
