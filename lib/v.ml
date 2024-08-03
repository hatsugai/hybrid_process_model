open Printf
open Ppx_hash_lib.Std.Hash.Builtin
open S
open Col

type builtin_id = int
[@@deriving show { with_path=false }, eq, ord, hash]

type loc = int
[@@deriving show { with_path=false }, eq, ord, hash]

type t =
  Ref of loc
| Unit
| Bool of bool
| Int of int
| Tuple of t list
| Constructor of Id.t
| Variant of { ctor : Id.t; args : t list }
| Record of { fields : t IdMap.t }
| Fun of { name : Id.t; params : Id.t list; stmt_id : U.stmt_id; env : t IdMap.t }
| Builtin of builtin_id
[@@deriving show { with_path=false }, eq, ord, hash]
