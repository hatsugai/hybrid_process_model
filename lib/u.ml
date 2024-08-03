open Printf
open Ppx_hash_lib.Std.Hash.Builtin

type expr_id = int
[@@deriving show { with_path=false }, eq, ord, hash]
type stmt_id = int
[@@deriving show { with_path=false }, eq, ord, hash]
type syncterm_id = int
[@@deriving show { with_path=false }, eq, ord, hash]

type expr =
  Unit
| Bool of bool
| Int of int
| Var of Id.t
| Ref of expr_id
| Deref of expr_id
| And of expr_id * expr_id
| Or of expr_id * expr_id
| Tuple of expr_id list
| IfExpr of expr_id * expr_id * expr_id
| CaseExpr of expr_id * (Id.t * (Id.t list * expr_id)) list
| Fun of Id.t * Id.t list * stmt_id
| Apply of expr_id * expr_id list
| RecordRef of expr_id * Id.t
| RecordCons of (Id.t * expr_id) list
| RecordUpdate of expr_id * (Id.t * expr_id) list
[@@deriving show { with_path=false }, eq, ord]

and stmt =
  Skip
| Assign of Id.t * expr_id
| RAssign of expr_id * expr_id
| Let of Id.t * expr_id
| Detuple of Id.t list * expr_id
| Seq of stmt_id list
| If of expr_id * stmt_id * stmt_id
| Case of expr_id * (Id.t * (Id.t list * stmt_id)) list
| While of expr_id * stmt_id
| Return of expr_id
| Break
| Continue
| Select of syncterm_id list
[@@deriving show { with_path=false }, eq, ord]

and syncterm =
  Event of expr_id * stmt_id
| Receive of expr_id * Id.t * stmt_id
[@@deriving show { with_path=false }, eq, ord]

module StmtMap =
  struct
    include Map.Make (struct
                type t = S.stmt
                let compare = S.compare_stmt
              end)
    let show m =
      (fold
         (fun stmt id acc ->
           acc ^ (sprintf "(%s, %d) " (S.show_stmt stmt) id))
         m "{ ") ^ "}"
  end

module ExprMap =
  struct
    include Map.Make (struct
                type t = S.expr
                let compare = S.compare_expr
              end)
    let show m =
      (fold
         (fun expr id acc ->
           acc ^ (sprintf "(%s, %d) " (S.show_expr expr) id))
         m "{ ") ^ "}"
  end

module SynctermMap = Map.Make (struct
                     type t = S.syncterm
                     let compare = S.compare_syncterm
                     end)

type syntax_id_map = {
    stmt_map : stmt_id StmtMap.t;
    expr_map : expr_id ExprMap.t;
    syncterm_map : syncterm_id SynctermMap.t;
}

let rec walk_stmt (m : syntax_id_map) (stmt : S.stmt) =
  let m = 
    match stmt with
      Skip -> m
    | Assign (x, e) -> walk_expr m e
    | RAssign (e1, e2) -> walk_expr (walk_expr m e1) e2
    | Let (x, e) -> walk_expr m e
    | Detuple (xs, e) -> walk_expr m e
    | Seq stmt_list -> List.fold_left walk_stmt m stmt_list
    | If (test, stmt1, stmt2) ->
       walk_stmt (walk_stmt (walk_expr m test) stmt1) stmt2
    | Case (e, branches) ->
       List.fold_left
         (fun m (ctor, (params, stmt)) -> walk_stmt m stmt)
         (walk_expr m e) branches
    | While (e, s) -> walk_stmt (walk_expr m e) s
    | Return e -> walk_expr m e
    | Break -> m
    | Continue -> m
    | Select syncterm_list -> List.fold_left walk_syncterm m syncterm_list
  in
  if StmtMap.mem stmt m.stmt_map then
    m
  else
    let i = StmtMap.cardinal m.stmt_map in
    { m with stmt_map = StmtMap.add stmt i m.stmt_map }

and walk_expr (m : syntax_id_map) (expr : S.expr) =
  (* printf "walk_expr >> expr=%s, expr_map=%s\n" (S.show_expr expr) (ExprMap.show m.expr_map); *)
  let m = walk_expr2 m expr in
  (* printf "walk_expr << expr=%s, expr_map=%s\n" (S.show_expr expr) (ExprMap.show m.expr_map); *)
  m

and walk_expr2 (m : syntax_id_map) (expr : S.expr) =
  let m = 
    match expr with
      Unit -> m
    | Bool b -> m
    | Int k -> m
    | Var x -> m
    | Ref e -> walk_expr m e
    | Deref e -> walk_expr m e
    | And (e1, e2) -> walk_expr (walk_expr m e1) e2
    | Or (e1, e2) -> walk_expr (walk_expr m e1) e2
    | Tuple es -> List.fold_left walk_expr m es
    | IfExpr (test, e1, e2) -> List.fold_left walk_expr m [test; e1; e2]
    | CaseExpr (e, branches) ->
       List.fold_left
         (fun m (ctor, (params, e)) -> walk_expr m e)
         (walk_expr m e) branches
    | Fun (name, params, stmt) -> walk_stmt m stmt
    | Apply (f, args) ->
       List.fold_left walk_expr (walk_expr m f) args
    | RecordRef (record, field_name) -> walk_expr m record
    | RecordCons fields ->
       List.fold_left (fun m (field_name, e) -> walk_expr m e) m fields
    | RecordUpdate (record, fields) ->
       List.fold_left (fun m (field_name, e) -> walk_expr m e) (walk_expr m record) fields
  in
  if ExprMap.mem expr m.expr_map then
    m
  else
    let i = ExprMap.cardinal m.expr_map in
    { m with expr_map = ExprMap.add expr i m.expr_map }

and walk_syncterm (m : syntax_id_map) (syncterm : S.syncterm) =
  let m =
    match syncterm with
      Event (event, stmt) ->
       walk_stmt (walk_expr m event) stmt
    | Receive (channel, param, stmt) ->
       walk_stmt (walk_expr m channel) stmt
  in
  if SynctermMap.mem syncterm m.syncterm_map then
    m
  else
    let i = SynctermMap.cardinal m.syncterm_map in
    { m with syncterm_map = SynctermMap.add syncterm i m.syncterm_map }

let stmt_id (m : syntax_id_map) (stmt : S.stmt) =
  StmtMap.find stmt m.stmt_map

let expr_id (m : syntax_id_map) (expr : S.expr) =
  ExprMap.find expr m.expr_map

let syncterm_id (m : syntax_id_map) (syncterm : S.syncterm) =
  SynctermMap.find syncterm m.syncterm_map

let conv_stmt (m : syntax_id_map) (stmt : S.stmt) : stmt =
  match stmt with
    Skip -> Skip
  | Assign (x, e) -> Assign (x, expr_id m e)
  | RAssign (e1, e2) -> RAssign (expr_id m e1, expr_id m e2)
  | Let (x, e) -> Let (x, expr_id m e)
  | Detuple (xs, e) -> Detuple (xs, expr_id m e)
  | Seq stmt_list -> Seq (List.map (stmt_id m) stmt_list)
  | If (test, stmt1, stmt2) ->
     If (expr_id m test, stmt_id m stmt1, stmt_id m stmt2)
  | Case (e, branches) ->
     Case (expr_id m e,
           List.map (fun (ctor, (params, stmt)) -> (ctor, (params, stmt_id m stmt)))
             branches)
  | While (e, s) -> While (expr_id m e, stmt_id m s)
  | Return e -> Return (expr_id m e)
  | Break -> Break
  | Continue -> Continue
  | Select syncterm_list -> Select (List.map (syncterm_id m) syncterm_list)

let conv_expr (m : syntax_id_map) (expr : S.expr) : expr =
  match expr with
    Unit -> Unit
  | Bool b -> Bool b
  | Int k -> Int k
  | Var x -> Var x
  | Ref e -> Ref (expr_id m e)
  | Deref e -> Deref (expr_id m e)
  | And (e1, e2) -> And (expr_id m e1, expr_id m e2)
  | Or (e1, e2) -> Or (expr_id m e1, expr_id m e2)
  | Tuple es -> Tuple (List.map (expr_id m) es)
  | IfExpr (test, e1, e2) -> IfExpr (expr_id m test, expr_id m e1, expr_id m e2)
  | CaseExpr (e, branches) ->
     CaseExpr (expr_id m e,
               List.map
                 (fun (ctor, (params, e)) -> (ctor, (params, expr_id m e)))
                 branches)
  | Fun (name, params, stmt) -> Fun (name, params, stmt_id m stmt)
  | Apply (f, args) ->
     Apply (expr_id m f, List.map (expr_id m) args)
  | RecordRef (record, field_name) -> RecordRef (expr_id m record, field_name)
  | RecordCons fields ->
     RecordCons (List.map (fun (field_name, e) -> (field_name, expr_id m e)) fields)
  | RecordUpdate (record, fields) ->
     RecordUpdate (expr_id m record, 
                   List.map (fun (field_name, e) -> (field_name, expr_id m e)) fields)

and conv_syncterm (m : syntax_id_map) (syncterm : S.syncterm) : syncterm =
  match syncterm with
    Event (event, stmt) ->
     Event (expr_id m event, stmt_id m stmt)
  | Receive (channel, param, stmt) ->
     Receive (expr_id m channel, param, stmt_id m stmt)

let map_to_array m =
  let n = ExprMap.cardinal m.expr_map in
  let ve = Array.make n Unit in
  ExprMap.iter (fun e i -> ve.(i) <- conv_expr m e) m.expr_map;
  let n = StmtMap.cardinal m.stmt_map in
  let vs = Array.make n Skip in
  StmtMap.iter (fun e i -> vs.(i) <- conv_stmt m e) m.stmt_map;
  let n = SynctermMap.cardinal m.syncterm_map in
  let vy = Array.make n (Event (0, 0)) in
  SynctermMap.iter (fun e i -> vy.(i) <- conv_syncterm m e) m.syncterm_map;
  (ve, vs, vy)

let make_maps () =
  {
    stmt_map = StmtMap.empty;
    expr_map = ExprMap.empty;
    syncterm_map = SynctermMap.empty
  }

let make_vec_expr (expr : S.expr) =
  let m = make_maps () in
  let m = walk_expr m expr in
  (m, map_to_array m)

let make_vec_stmt (stmt : S.stmt) =
  let m = make_maps () in
  let m = walk_stmt m stmt in
  (m, map_to_array m)

let make_vec_defs (def_list : S.definition list) =
  let m = make_maps () in
  let m =
    List.fold_left
      (fun m def ->
        match def with
          S.Constant (name, expr) -> walk_expr m expr
        | S.Variable (name, expr) -> walk_expr m expr
        | S.Function (name, params, stmt) -> walk_stmt m stmt)
      m def_list
  in
  (m, map_to_array m)
