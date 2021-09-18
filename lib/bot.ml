open Core
open Async
open Cohttp
open Cohttp_async

let server =
  let%map_open.Command () = return () in
  fun () ->
    let callback ~body addr req =
      let uri = Uri.to_string (Request.uri req) in
      let meth = Request.meth req in
      let%bind body = Body.to_string body in
      Log.Global.info_s
        [%message (addr : Socket.Address.Inet.t) uri (meth : Code.meth) body];
      Server.respond_string "<h1>Hello from fp.vasalf.net</h1>"
    in
    let where_to_listen = Tcp.Where_to_listen.of_port 8179 in
    let%bind server = Server.create ~on_handler_error:`Raise where_to_listen callback in
    ignore server;
    Deferred.never ()
;;

let command = Command.async ~summary:"Run the bot" server
