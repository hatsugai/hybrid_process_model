open Error
open Col
open Exec

module P = Process.Transf

let transf v_to_event v_to_ch e_to_v make_call_event make_ret_event d
      ((c, k) : State.t)
    : ('e, 'ch, State.t) P.sync_term list =
  match psk d k c with
    ResSync (c, sync_list, sk) ->
     List.map
       (fun sync ->
         match sync with
           Event { event; cont } ->
            P.Event (v_to_event event, (c, cont))
         | Receive { channel; param; receive_cont } ->
            P.Receive (v_to_ch channel, P.guard_true,
                       continue_receive e_to_v d c receive_cont param))
       sync_list
  | ResCall { context; name; args; cont } ->
     let event = make_call_event name args in
     [P.Event (event, (context, cont))]
  | ResReturn { context; name; retval; cont } ->
     let event = make_ret_event name retval in
     [P.Event (event, (context, cont))]
  | ResScCont c -> error __FUNCTION__
  | ResEcCont (c, v) -> error __FUNCTION__

let get_initial_state d c =
  match IdHash.find_opt d.cdefs (Id.make "main") with
    Some (V.Fun f) -> (c, ScStmt (f.stmt_id, ScCont))
  | _ -> error __FUNCTION__
