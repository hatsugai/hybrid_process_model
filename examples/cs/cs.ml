open Process
open Hybrid_process_model
open Error

module P = Process.Transf

module Event =
  struct
    type t =
      (* user <-> app *)
      UserOperation of V.t
    | Display of V.t
    (* app <-> server *)
    | HttpRequest of V.t
    | HttpResponse of V.t
    (* app internals *)
    | Request of V.t
    | Notify of V.t
    (* function call/ret *)
    | AppEventHandlerCall of V.t
    | AppEventHandlerRet of V.t
    | InvokeHttpRequstCall of V.t
    | InvokeHttpRequstRet of V.t
    | ShowContentsCall of V.t
    | ShowContentsRet of V.t
    | CallbackCall of V.t
    | CallbackRet of V.t
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

module Channel =
  struct
    type t =
      UserOperation
    | Display
    | HttpRequest
    | HttpResponse
    | Request
    | Notify
    | AppEventHandlerCall
    | AppEventHandlerRet
    | InvokeHttpRequstCall
    | InvokeHttpRequstRet
    | ShowContentsCall
    | ShowContentsRet
    | CallbackCall
    | CallbackRet
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

let event_to_ch (e : Event.t) : Channel.t =
  match e with
  | UserOperation _ -> UserOperation
  | Display _ -> Display
  | HttpRequest _ -> HttpRequest
  | HttpResponse _ -> HttpResponse
  | Request _ -> Request
  | Notify _ -> Notify
  | AppEventHandlerCall _ -> AppEventHandlerCall
  | AppEventHandlerRet _ -> AppEventHandlerRet
  | InvokeHttpRequstCall _ -> InvokeHttpRequstCall
  | InvokeHttpRequstRet _ -> InvokeHttpRequstRet
  | ShowContentsCall _ -> ShowContentsCall
  | ShowContentsRet _ -> ShowContentsRet
  | CallbackCall _ -> CallbackCall
  | CallbackRet _ -> CallbackRet

module Server =
  struct
    type t = S0 | S1 of V.t
    [@@deriving show { with_path=false }, eq, ord, hash]

    open Process.Transf

    let transf s =
      match s with
        S0 -> [Receive (Channel.HttpRequest, guard_true,
                        (fun (Event.HttpRequest v) -> S1 v)
                          [@ocaml.warning "-8"])]
      | S1 v -> [Event (Event.HttpResponse v, S0)]
  end

module BackgroundWorker =
  struct
    type t = S0 | S1 of V.t | S2 | S3 of V.t
    [@@deriving show { with_path=false }, eq, ord, hash]

    let transf s =
      match s with
        S0 -> [P.Receive (Channel.Request, P.guard_true,
                        (fun (Event.Request v) -> S1 v)
                          [@ocaml.warning "-8"])]
      | S1 v -> [Event (Event.HttpRequest v, S2)]
      | S2 -> [Receive (Channel.HttpResponse, P.guard_true,
                        (fun (Event.HttpResponse v) -> S3 v)
                          [@ocaml.warning "-8"])]
      | S3 v -> [Event (Event.Notify v, S0)]
  end

let ch_apply (ch : Channel.t) x : Event.t =
  match ch with
  | UserOperation -> UserOperation x
  | Display -> Display x
  | HttpRequest -> HttpRequest x
  | HttpResponse -> HttpResponse x
  | Request -> Request x
  | Notify -> Notify x
  | AppEventHandlerCall -> AppEventHandlerCall x
  | AppEventHandlerRet -> AppEventHandlerRet x
  | InvokeHttpRequstCall -> InvokeHttpRequstCall x
  | InvokeHttpRequstRet -> InvokeHttpRequstRet x
  | ShowContentsCall -> ShowContentsCall x
  | ShowContentsRet -> ShowContentsRet x
  | CallbackCall -> CallbackCall x
  | CallbackRet -> CallbackRet x

let chs =
  List.map (fun (ch : Channel.t) -> (Channel.show ch, ch))
    [UserOperation; Display; HttpRequest; HttpResponse; Request; Notify]

let calls = [
    ("app_event_handler", Channel.AppEventHandlerCall);
    ("invoke_http_request", InvokeHttpRequstCall);
    ("show_contents", ShowContentsCall);
    ("callback", CallbackCall)]

let rets = [
    ("app_event_handler", Channel.AppEventHandlerRet);
    ("invoke_http_request", InvokeHttpRequstRet);
    ("show_contents", ShowContentsRet);
    ("callback", CallbackRet)]

let ch_to_event_list (ch : Channel.t) =
  match ch with
    UserOperation -> [Event.UserOperation V.Unit]
  | _ -> []

let v_to_event v =
  let open V in
  match v with
    Variant { ctor; args } ->
     let ch = List.assoc ctor chs in
     ch_apply ch (List.hd args)
  | _ -> error __FUNCTION__

let v_to_ch v =
  let open V in
  match v with
    Constructor name ->
     List.assoc name chs
  | _ -> error __FUNCTION__

let e_to_v (e : Event.t) =
  match e with
    UserOperation v -> v
  | Display v -> v
  | HttpRequest v -> v
  | HttpResponse v -> v
  | Request v -> v
  | Notify v -> v
  | AppEventHandlerCall v -> v
  | AppEventHandlerRet v -> v
  | InvokeHttpRequstCall v -> v
  | InvokeHttpRequstRet v -> v
  | ShowContentsCall v -> v
  | ShowContentsRet v -> v
  | CallbackCall v -> v
  | CallbackRet v -> v

let make_call_event name args =
  let ch = List.assoc name calls in
  ch_apply ch (List.hd args)

let make_ret_event name retval =
  let ch = List.assoc name rets in
  ch_apply ch retval

let constructors = 
  [
    "UserOperation"; "Display";
    "Request"; "Notify";
    "HttpRequest"; "HttpResponse"
  ]

let program = {heterodyne_program|

var callback = &0

main () {
  while true do {
    select
      UserOperation ? x -> let z = app_event_handler (x)
    | Notify ? x -> let z = (*callback)(x)
    end
  }
}

invoke_http_request(cb) {
  callback := cb;
  Request ! 0 -> skip
}

show_contents(x) {
  Display ! x -> skip
}

app_event_handler(x) {
  let z = invoke_http_request(fun callback (x) let z = show_contents(x))
}

|heterodyne_program}

let app_transf () =
  let def_list = Parse.parse program in
  let (d, c) = Init.make_d_c_defs ~constructors def_list in
  let transf = Transf.transf v_to_event v_to_ch e_to_v make_call_event make_ret_event d in
  let initial_state = Transf.get_initial_state d c in
  d.trace <- true;
  Col.IdHash.replace d.observ (Id.make "app_event_handler") Both;
  Col.IdHash.replace d.observ (Id.make "invoke_http_request") Both;
  Col.IdHash.replace d.observ (Id.make "show_contents") Both;
  Col.IdHash.replace d.observ (Id.make "callback") Both;
  (transf, initial_state)

module App =
  struct
    type t = Exec.State.t * BackgroundWorker.t
    [@@deriving show { with_path=false }, eq, ord, hash]

    let sync (ch : Channel.t) =
      match ch with
        Request | Notify -> true
        | _ -> false

    let transf =
      let (transf, _) = app_transf () in
      let module C = Process.Transf.Composition (Event) (Channel) in
      C.composition event_to_ch sync transf BackgroundWorker.transf

    let s0 = 
      let (_, app_s0) = app_transf () in (app_s0, BackgroundWorker.S0)
  end

module System =
  struct
    type t = App.t * Server.t
    [@@deriving show { with_path=false }, eq, ord, hash]

    let sync (ch : Channel.t) =
      match ch with
        HttpRequest | HttpResponse -> true
        | _ -> false

    let transf =
      let module C = Process.Transf.Composition (Event) (Channel) in
      C.composition event_to_ch sync App.transf Server.transf

    let s0 = (App.s0, Server.S0)
  end

let sim_app () =
  let module S = SimTransf.Make (Event) (Channel) (Exec.State) in
  let (transf, initial_state) = app_transf () in
  S.simulation ch_to_event_list transf initial_state

let sim () =
  let module S = SimTransf.Make (Event) (Channel) (System) in
  S.simulation ch_to_event_list System.transf System.s0

let () =
  sim ()
