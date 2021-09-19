open Core
open Async
open Cohttp
open Cohttp_async
open Types

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
  Log.Global.info_s [%message (headers : Header.t) (body : Body.t)];
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
    Mirage_crypto_rng_unix.initialize ();
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
