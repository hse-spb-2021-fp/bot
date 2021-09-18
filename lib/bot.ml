open Core
open Async
open Cohttp
open Cohttp_async

let process_hook body _req =
  let%bind body = Body.to_string body in
  (match Event.of_string body with
  | Ok (Pull_request pr) ->
    Log.Global.info_s [%message "Pull request" (pr : Event.Pull_request.t)]
  | Error e -> Log.Global.error_s [%message "Error" (e : Error.t)]);
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
