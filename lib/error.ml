exception Error of string
let error s = raise (Error s)
let error1 s m = raise (Error (s ^ m))
