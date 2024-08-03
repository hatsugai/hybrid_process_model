open Error
open Col

let builtin_literals = ["Nil"]
let builtin_constructors = ["Cons"]

let builtin_functions = [
    ("+", fun vs ->
          match vs with
            [V.Int v1; V.Int v2] -> V.Int (v1 + v2)
          | _ -> error "+");
    ("-", fun vs ->
      match vs with
        [V.Int v1; V.Int v2] -> V.Int (v1 - v2)
      | _ -> error "-");
    ("*", fun vs ->
          match vs with
            [V.Int v1; V.Int v2] -> V.Int (v1 * v2)
          | _ -> error "*");
    ("=", fun vs ->
      match vs with
        [V.Int v1; V.Int v2] -> V.Bool (v1 = v2)
      | _ -> error "=");
    ("<", fun vs ->
      match vs with
        [V.Int v1; V.Int v2] -> V.Bool (v1 < v2)
      | _ -> error "<");
    ("<=", fun vs ->
      match vs with
        [V.Int v1; V.Int v2] -> V.Bool (v1 <= v2)
      | _ -> error "<=");
    ]

let reg_literals cdefs literals =
  List.iter
    (fun ctor ->
      IdHash.replace cdefs ctor (V.Variant { ctor; args = [] }))
    literals

let reg_constructors cdefs constructors =
  List.iter
    (fun ctor ->
      IdHash.replace cdefs ctor (V.Constructor ctor))
    constructors

let reg_builtins cdefs =
  let n = List.length builtin_functions in
  let vbf = Array.make n (fun _ -> V.Unit) in
  List.iteri
    (fun i (name, f) ->
      IdHash.replace cdefs name (V.Builtin i);
      vbf.(i) <- f)
    builtin_functions;
  vbf

let init cdefs =
  reg_literals cdefs builtin_literals;
  reg_constructors cdefs builtin_constructors;
  reg_builtins cdefs
