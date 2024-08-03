open Printf
open Base.Hash.Builtin
open Process
open Hybrid_process_model
open Error

module Event =
  struct
    type t =
      HttpRequest of int
    | HttpResponse of int
    | FooCall of int
    | FooRet of int
    | BarCall of int
    | BarRet of int
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

module Channel =
  struct
    type t =
      HttpRequest
    | HttpResponse
    | FooCall
    | FooRet
    | BarCall
    | BarRet
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

let ch_alist =
  let open Channel in
  List.map (fun ch -> (show ch, ch))
    [HttpRequest; HttpResponse; FooCall; FooRet; BarCall; BarRet]

let ch_apply (ch : Channel.t) x : Event.t =
  match ch with
  | HttpRequest -> HttpRequest x
  | HttpResponse -> HttpResponse x
  | FooCall -> FooCall x
  | FooRet -> FooRet x
  | BarCall -> BarCall x
  | BarRet -> BarRet x

let get_int v =
  let open V in
  printf "get_int: %s\n" (V.show v);
  match v with
    Int k -> k
  | _ -> error __FUNCTION__

let ch_to_event_list (ch : Channel.t) =
  match ch with
    HttpResponse ->
     List.map (fun x -> Event.HttpResponse x) [0; 1; 2]
  | _ -> []

let v_to_event v =
  let open V in
  match v with
    Variant { ctor; args } ->
     let k = get_int (List.hd args) in
     let ch = List.assoc ctor ch_alist in
     ch_apply ch k
  | _ -> error __FUNCTION__

let v_to_ch v =
  let open V in
  match v with
    Constructor name ->
     List.assoc name ch_alist
  | _ -> error __FUNCTION__

let e_to_v (e : Event.t) =
  let x =
    match e with
      HttpRequest x -> x
    | HttpResponse x -> x
    | FooCall x -> x
    | FooRet x -> x
    | BarCall x -> x
    | BarRet x -> x
  in
  V.Int x

let make_call_event name args =
  let open Channel in
  let k = get_int (List.hd args) in
  let ch = List.assoc name
             [("foo", FooCall); ("bar", BarCall)]
  in
  ch_apply ch k

let make_ret_event name retval =
  let open Channel in
  let k = get_int retval in
  let ch = List.assoc name
             [("foo", FooRet); ("bar", BarRet)]
  in
  ch_apply ch k

let constructors = ["HttpRequest"; "HttpResponse"]

let program = {heterodyne_program|
main () {
  let x = &0;
  while true do {
    x := foo(*x)
  }
}

foo(x) {
  return bar(x) + 1
}

bar(x) {
  HttpRequest ! x -> HttpResponse ? y -> return y
}
|heterodyne_program}

let sim () =
  let def_list = Parse.parse program in
  let (d, c) = Init.make_d_c_defs ~constructors def_list in
  let module S = SimTransf.Make (Event) (Channel) (Exec.State) in
  let transf = Transf.transf v_to_event v_to_ch e_to_v make_call_event make_ret_event d in
  let initial_state = Transf.get_initial_state d c in
  d.trace <- true;
  Col.IdHash.replace d.observ (Id.make "foo") Both;
  Col.IdHash.replace d.observ (Id.make "bar") Both;
  S.simulation ch_to_event_list transf initial_state

let () =
  sim ()
