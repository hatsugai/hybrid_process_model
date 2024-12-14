open Printf
open Ppx_hash_lib.Std.Hash.Builtin
open Error
open Col

type expr_id = U.expr_id
[@@deriving show { with_path=false }, eq, ord, hash]
type stmt_id = U.stmt_id
[@@deriving show { with_path=false }, eq, ord, hash]
type syncterm_id = U.syncterm_id
[@@deriving show { with_path=false }, eq, ord, hash]
type buildin_id = V.builtin_id
[@@deriving show { with_path=false }, eq, ord, hash]

module LocMap =
  struct
    include Map.Make (
                struct
                  type t = V.loc
                  let compare = compare
                end)
    let hash_fold_t f s m =
      fold (fun k v s -> f (V.hash_fold_loc s k) v)
        m s
    let pp pp_v fmt m =
      iter (fun k v ->
          Format.fprintf fmt "(";
          V.pp_loc fmt k; 
          Format.fprintf fmt ",";
          pp_v fmt v;
          Format.fprintf fmt ")")
        m
    let show show_v m =
      let buf = Buffer.create 0 in
      iter (fun k v -> bprintf buf "(%s, %s)" (V.show_loc k) (show_v v)) m;
      Buffer.contents buf
    let alloc m =
      let s = to_rev_seq m in
      match s () with
        Seq.Nil -> 0
      | Seq.Cons ((loc_max, _), _) -> loc_max + 1
  end

type expr_cont =
  EcCont
| EcAssign of V.loc * stmt_cont
| EcRAssign1 of expr_id * stmt_cont
| EcRAssign2 of V.loc * stmt_cont
| EcRef of expr_cont
| EcDeref of expr_cont
| EcAnd of expr_id * expr_cont
| EcOr of expr_id * expr_cont
| EcIf of stmt_id * stmt_id * stmt_cont
| EcCase of (Id.t * (Id.t list * stmt_id)) list * stmt_cont
| EcIfExpr of expr_id * expr_id * expr_cont
| EcCaseExpr of (Id.t * (Id.t list * expr_id)) list * expr_cont
| EcReturn
| EcWhile of expr_id * stmt_id * stmt_cont
| EcEvlis of V.t list * expr_id list * evlis_cont
| EcSynctermEvent of sync list * syncterm_id list * stmt_cont * stmt_id
| EcSynctermReceive of sync list * syncterm_id list * stmt_cont * Id.t * stmt_id
| EcApply1 of expr_id list * expr_cont
| EcLet of Id.t * stmt_cont
| EcDetuple of Id.t list * stmt_cont
| EcRecordRef of Id.t * expr_cont
| EcRecordUpdate1 of (Id.t * expr_id) list * expr_cont
[@@deriving show { with_path=false }, eq, ord, hash]

and stmt_cont =
  ScCont
| ScStmt of stmt_id * stmt_cont
| ScSeq of stmt_id list * stmt_cont
| ScWhile of expr_id * stmt_id * stmt_cont
| ScReturn
| ScEventCall of
    expr_cont
    * Id.t * Id.t list * stmt_id * V.t IdMap.t
    * V.t list
| ScEventReturn of expr_cont * V.t
[@@deriving show { with_path=false }, eq, ord, hash]

and evlis_cont =
| EcApply2 of V.t * expr_cont
| EcTuple of expr_cont
| EcRecordCons of Id.t list * expr_cont
| EcRecordUpdate2 of V.t IdMap.t * Id.t list * expr_cont
[@@deriving show { with_path=false }, eq, ord, hash]

and sync =
  Event of { event : V.t; cont : stmt_cont }
| Receive of { channel : V.t; param : Id.t; receive_cont : receive_cont }
[@@deriving show { with_path=false }, eq, ord, hash]

and receive_cont = stmt_id * stmt_cont

type while_context = {
    test : expr_id;
    stmt_id : stmt_id;
    scont : stmt_cont;
}
[@@deriving show { with_path=false }, eq, ord, hash]

type call_stack = {
    name : Id.t;
    le : V.t IdMap.t;
    wl : while_context list;
    econt : expr_cont;
  }
[@@deriving show { with_path=false }, eq, ord, hash]

type context = {
    ge : V.t IdMap.t;
    le : V.t IdMap.t;
    he : V.t LocMap.t;
    cs : call_stack list;
    wl : while_context list;
}
[@@deriving show { with_path=false }, eq, ord, hash]

module State =
  struct
    type t = context * stmt_cont
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

type result =
  ResSync of context * sync list * stmt_cont
| ResScCont of context
| ResEcCont of context * V.t
| ResCall of { context : context;
               name : Id.t;
               args : V.t list;
               cont : stmt_cont }
| ResReturn of { context : context;
                 name : Id.t;
                 retval : V.t;
                 cont : stmt_cont }
[@@deriving show { with_path=false }]

type observ = Call | Return | Both

type def_context = {
    cdefs : V.t IdHash.t;
    observ : observ IdHash.t;
    mutable trace : bool;
    m : U.syntax_id_map;
    vs : U.stmt array;
    ve : U.expr array;
    vsy : U.syncterm array;
    vbf : (V.t list -> V.t) array;
}

let extend_local_env c xs vs =
  let le =
    List.fold_left2
      (fun le x v -> IdMap.add x v le)
      c.le xs vs
  in
  { c with le }

let rec exec
          (d : def_context)
          (c : context)
          (stmt_id : stmt_id)
          (k : stmt_cont)
        : result =
  let stmt = d.vs.(stmt_id) in
  (if d.trace then
     printf "exec [%d,%d] %s %s %s %s %s\n"
       (List.length c.cs)
       (List.length c.wl)
       (U.show_stmt stmt)
       (IdMap.show V.show c.le)
       (IdMap.show V.show c.ge)
       (LocMap.show V.show c.he)
       (show_stmt_cont k));
  match stmt with
    Skip -> psk d k c
  | Assign (x, e) ->
     (match IdMap.find_opt x c.le with
        Some v ->
         (match v with
            Ref loc -> eval d c e (EcAssign (loc, k))
          | _ -> error1 "Assign" (Id.show x))
      | None ->
         (match IdMap.find_opt x c.ge with
            Some v ->
             (match v with
                Ref loc -> eval d c e (EcAssign (loc, k))
              | _ -> error1 "Assign" (Id.show x))
          | None -> error1 "Assign" (Id.show x)))
  | RAssign (e1, e2) ->
     eval d c e1 (EcRAssign1 (e2, k))
  | Let (x, e) ->
     eval d c e (EcLet (x, k))
  | Detuple (xs, e) ->
     eval d c e (EcDetuple (xs, k))
  | Seq stmt_list ->
     execs d c stmt_list k
  | If (test, stmt1, stmt2) ->
     eval d c test (EcIf (stmt1, stmt2, k))
  | Case (e, branches) ->
     eval d c e (EcCase (branches, k))
  | While (test, stmt_id) ->
     let c = { c with wl = { test; stmt_id; scont = k }::c.wl } in
     exec_while d c test stmt_id k
  | Break ->
     (match c.wl with
        { scont; test; stmt_id }::wl -> psk d scont { c with wl }
      | _ -> error "break")
  | Continue ->
     (match c.wl with
        { test; stmt_id; scont = k }::wl ->
         exec_while d c test stmt_id k
      | _ -> error "continue")
  | Return e ->
     eval d c e EcReturn
  | Select syncterm_list ->
     eval_syncterm_list d c [] syncterm_list k

and eval_syncterm_list d c rs ss k =
  match ss with
    [] -> ResSync (c, rs, k)
  | syncterm::ss ->
     (match d.vsy.(syncterm) with
        Event (event, stmt) ->
         eval d c event (EcSynctermEvent (rs, ss, k, stmt))
      | Receive (channel, x, stmt) ->
         eval d c channel (EcSynctermReceive (rs, ss, k, x, stmt)))

and execs d c stmt_list k =
  match stmt_list with
    [] -> psk d k c
  | stmt::stmt_list ->
     exec d c stmt (ScSeq (stmt_list, k))

and exec_while d c test stmt_id k =
  eval d c test (EcWhile (test, stmt_id, k))

and eval (d : def_context)
(c : context)
(expr_id : expr_id)
(k : expr_cont)
    : result =
  let expr = d.ve.(expr_id) in
  (if d.trace then
     printf "eval [%d,%d] %s %s %s %s %s\n"
       (List.length c.cs) (List.length c.wl)
       (U.show_expr expr)
       (IdMap.show V.show c.le)
       (IdMap.show V.show c.ge)
       (LocMap.show V.show c.he)
       (show_expr_cont k));
  match expr with
    Unit -> pek d k c (V.Unit)
  | Bool b -> pek d k c (V.Bool b)
  | Int i -> pek d k c (V.Int i)
  | Var x ->
     (match IdMap.find_opt x c.le with
        Some v -> pek d k c v
      | None ->
         (match IdMap.find_opt x c.ge with
            Some v -> pek d k c v
          | None ->
             (match IdHash.find_opt d.cdefs x with
                Some v -> pek d k c v
              | None ->
                 error1 "unbound var: " (Id.show x))))
  | Ref e ->
     eval d c e (EcRef k)
  | Deref e ->
     eval d c e (EcDeref k)
  | And (e1, e2) ->
     eval d c e1 (EcAnd (e2, k))
  | Or (e1, e2) ->
     eval d c e1 (EcOr (e2, k))
  | Tuple es ->
     evlis d c [] es (EcTuple k)
  | IfExpr (test, e1, e2) ->
     eval d c test (EcIfExpr (e1, e2, k))
  | CaseExpr (e, branches) ->
     eval d c e (EcCaseExpr (branches, k))
  | Fun (name, xs, stmt_id) ->
     pek d k c (V.Fun { name; params = xs; stmt_id; env = c.le })
  | Apply (f, args) ->
     eval d c f (EcApply1 (args, k))
  | RecordRef (e, field) ->
     eval d c e (EcRecordRef (field, k))
  | RecordCons alist ->
     let fs = List.map fst alist in
     let es = List.map snd alist in
     evlis d c [] es (EcRecordCons (fs, k))
  | RecordUpdate (e, alist) ->
     eval d c e (EcRecordUpdate1 (alist, k))

and evlis d c rs es k =
  match es with
    [] -> pelk d k c (List.rev rs)
  | e::es ->
     eval d c e (EcEvlis (rs, es, k))

and pek d k c v =
  (if d.trace then
     printf "pek [%d,%d] %s %s %s %s\n"
       (List.length c.cs) (List.length c.wl)
       (V.show v)
       (IdMap.show V.show c.le)
       (IdMap.show V.show c.ge)
       (show_expr_cont k));
  match k with
    EcCont -> ResEcCont (c, v)
  | EcAssign (loc, k) ->
     psk d k { c with he = LocMap.add loc v c.he }
  | EcRAssign1 (e, k) ->
     (match v with
        Ref loc -> eval d c e (EcRAssign2 (loc, k))
      | _ -> error "RAssign")
  | EcRAssign2 (loc, k) ->
     let he = LocMap.add loc v c.he in
     psk d k { c with he }
  | EcRef k ->
     let loc = LocMap.alloc c.he in
     let he = LocMap.add loc v c.he in
     pek d k { c with he } (Ref loc)
  | EcDeref k ->
     (match v with
        Ref loc ->
         (match LocMap.find_opt loc c.he with
            Some v -> pek d k c v
          | None -> error "deref")
      | _ -> error "deref")
  | EcIf (stmt1, stmt2, k) ->
     (match v with
        Bool b ->
         exec d c (if b then stmt1 else stmt2) k
      | _ -> error "EcIf")
  | EcIfExpr (e1, e2, k) ->
     (match v with
        Bool b -> eval d c (if b then e1 else e2) k
      | _ -> error "EcIfExpr")
  | EcCase (branches, k) ->
     (match v with
        Variant { ctor; args } ->
         (match List.assoc_opt ctor branches with
            Some (params, stmt) ->
             let c = extend_local_env c params args in
             exec d c stmt k
          | None ->
             (match List.assoc_opt Id.wildcard branches with
                Some (_, stmt) ->
                 assert (args = []);
                 exec d c stmt k
              | None -> error "case"))
      | _ -> error "case")
  | EcCaseExpr (branches, k) ->
     (match v with
        Variant { ctor; args } ->
         (match List.assoc_opt ctor branches with
            Some (params, expr) ->
             let c = extend_local_env c params args in
             eval d c expr k
          | None ->
             (match List.assoc_opt Id.wildcard branches with
                Some (_, expr) ->
                 assert (args = []);
                 eval d c expr k
              | None -> error "case_expr"))
      | _ -> error "case_expr")
  | EcReturn ->
     (match c.cs with
        { name; le; wl; econt = k }::cs ->
         let c = { c with le; cs; wl } in
         (match IdHash.find_opt d.observ name with
            Some o when o = Return || o = Both ->
             let cont = ScEventReturn (k, v) in
             ResReturn { context = c; name; retval = v; cont }
          | _ -> pek d k c v)
      | _ -> error "return")
  | EcWhile (test, stmt, k) ->
     (match v with
        Bool b ->
         if b then
           exec d c stmt (ScWhile (test, stmt, k))
         else
           psk d k { c with wl = List.tl c.wl }
      | _ -> error "EcWhile")
  | EcEvlis (rs, es, k) ->
     (match es with
        [] -> pelk d k c (List.rev (v::rs))
      | e::es ->
         eval d c e (EcEvlis (v::rs, es, k)))
  | EcSynctermEvent (rs, ss, k, stmt_id) ->
     let sync = Event { event = v; cont = ScStmt (stmt_id, k) } in
     eval_syncterm_list d c (sync::rs) ss k
  | EcSynctermReceive (rs, ss, k, param, stmt_id) ->
     let sync = Receive { channel = v; param; receive_cont = (stmt_id, k) } in
     eval_syncterm_list d c (sync::rs) ss k
  | EcApply1 (args, k) ->
     evlis d c [] args (EcApply2 (v, k))
  | EcAnd (e, k) ->
     (match v with
        V.Bool false -> pek d k c (V.Bool false)
      | V.Bool true -> eval d c e k
      | _ -> error "EcAnd")
  | EcOr (e, k) ->
     (match v with
        V.Bool true -> pek d k c (V.Bool true)
      | V.Bool false -> eval d c e k
      | _ -> error "EcOr")
  | EcLet (x, k) ->
     let le = IdMap.add x v c.le in
     psk d k { c with le }
  | EcDetuple (xs, k) ->
     (match v with
        V.Tuple vs ->
         let le = List.fold_left2
                    (fun le x v -> IdMap.add x v le)
                    c.le xs vs
         in
         psk d k { c with le }
      | _ -> error "EcDetuple")
  | EcRecordRef (field, k) ->
     (match v with
        V.Record { fields } ->
         (match IdMap.find_opt field fields with
            Some v -> pek d k c v
          | None -> error "EcRecordRef")
      | _ -> error "EcRecordRef")
  | EcRecordUpdate1 (alist, k) ->
     (match v with
        V.Record { fields } ->
         assert (List.for_all (fun (f, _) -> IdMap.mem f fields) alist);
         let fs = List.map fst alist in
         let es = List.map snd alist in
         evlis d c [] es (EcRecordUpdate2 (fields, fs, k))
      | _ -> error "EcRecordUpdate1")

and pelk d k c vs =
  match k with
  | EcApply2 (f, k) ->
     (match f with
        Fun { name; params; stmt_id; env } ->
         pec d k c vs name params stmt_id env
      | Builtin id ->
         pek d k c (d.vbf.(id) vs)
      | Constructor ctor ->
         pek d k c (V.Variant { ctor; args = vs })
      | _ -> error "apply: not function")
  | EcTuple k ->
     pek d k c (V.Tuple vs)
  | EcRecordCons (fs, k) ->
     let fields =
       List.fold_left2
         (fun fields f v -> IdMap.add f v fields)
         IdMap.empty fs vs
     in
     pek d k c (V.Record { fields })
  | EcRecordUpdate2 (fields, fs, k) ->
     let fields =
       List.fold_left2
         (fun fields f v -> IdMap.add f v fields)
         IdMap.empty fs vs
     in
     pek d k c (V.Record { fields })

and pec d k c args name params stmt_id le =
  let le =
    List.fold_left2
      (fun le x v -> IdMap.add x v le)
      le params args
  in
  let c =
    { c with
      le;
      cs = ({ name; le = c.le; wl = c.wl; econt = k }::c.cs);
      wl = [] }
  in
  let cont = ScEventCall (k, name, params, stmt_id, le, args) in
  match IdHash.find_opt d.observ name with
    Some o when o = Call || o = Both ->
     ResCall { context = c; name; args; cont }
  | _ -> psk d cont c

and psk d k c =
  (if d.trace then
     printf "psk [%d,%d] %s %s %s\n"
       (List.length c.cs) (List.length c.wl)
       (IdMap.show V.show c.le)
       (IdMap.show V.show c.ge)
       (show_stmt_cont k));
  match k with
    ScCont -> ResScCont c
  | ScStmt (stmt_id, k) -> exec d c stmt_id k
  | ScSeq (stmt_list, k) -> execs d c stmt_list k
  | ScWhile (test, stmt, k) -> exec_while d c test stmt k
  | ScReturn ->
     (match c.cs with
        { name; le; wl; econt = k }::cs ->
         let c = { c with le; cs; wl } in
         (match IdHash.find_opt d.observ name with
            Some o when o = Return || o = Both ->
             let v = V.Unit in
             let cont = ScEventReturn (k, v) in
             ResReturn { context = c; name; retval = v; cont }
          | _ ->
             pek d k c V.Unit)
      | _ -> error "return")
  | ScEventCall (k, name, xs, stmt_id, le, vs) ->
     exec d c stmt_id ScReturn
  | ScEventReturn (k, v) ->
     pek d k c v

let continue_receive e_to_v d c (stmt_id, k) param e =
  let v = e_to_v e in
  let le = IdMap.add param v c.le in
  let c = { c with le } in
  let k = ScStmt (stmt_id, k) in
  (c, k)

