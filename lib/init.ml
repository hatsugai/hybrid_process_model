open Printf
open Error
open S
open Col
open Parse
open Exec
open Builtin

let make_c () = {
    ge = IdMap.empty;
    le = IdMap.empty;
    he = LocMap.empty;
    cs = [];
    wl = [];
  }

let make_cdefs literals constructors =
  let cdefs = IdHash.create 0 in
  reg_literals cdefs literals;
  reg_constructors cdefs constructors;
  cdefs

let make_d_expr ?(literals=[]) ?(constructors=[]) (expr : S.expr) =
  let cdefs = make_cdefs literals constructors in
  let vbf = Builtin.init cdefs in
  let (m, (ve, vs, vsy)) = U.make_vec_expr expr in
  {
    cdefs; m; ve; vs; vsy; vbf;
    observ = IdHash.create 0;
    trace = true;
  }

let make_d_stmt ?(literals=[]) ?(constructors=[]) (stmt : S.stmt) =
  let cdefs = make_cdefs literals constructors in
  let vbf = Builtin.init cdefs in
  let (m, (ve, vs, vsy)) = U.make_vec_stmt stmt in
  {
    cdefs; m; ve; vs; vsy; vbf;
    observ = IdHash.create 0;
    trace = true;
  }

let make_d_c_defs ?(literals=[]) ?(constructors=[])
      (def_list : S.definition list)
    : def_context * context =
  let cdefs = make_cdefs literals constructors in
  let vbf = Builtin.init cdefs in
  let (m, (ve, vs, vsy)) = U.make_vec_defs def_list in
  let c = make_c () in
  let d = {
      cdefs; vs; ve; vsy; vbf; m;
      observ = IdHash.create 0;
      trace = false;
    }
  in
  let c =
    List.fold_left
      (fun c def ->
        match def with
          S.Constant (name, expr) ->
           (match eval d c (U.expr_id m expr) EcCont with
              ResEcCont (c, v) -> IdHash.add cdefs name v; c
            | _ -> error "")
        | S.Variable (name, expr) ->
           (match eval d c (U.expr_id m expr) EcCont with
              ResEcCont (c, v) -> { c with ge = IdMap.add name v c.ge }
            | _ -> error "")
        | S.Function _ -> c)
      c def_list
  in
  List.iter
    (fun def ->
      match def with
        S.Function (name, params, stmt) ->
         let stmt_id = U.stmt_id m stmt in
         IdHash.replace cdefs name
           (V.Fun { name; params; stmt_id; env = IdMap.empty })
      | _ -> ())
    def_list;
  (d, c)
