open Ppx_hash_lib.Std.Hash.Builtin

type t = string
[@@deriving show { with_path=false }, eq, ord, hash]

let make s = s

let to_string s = s

let wildcard = make "_"
