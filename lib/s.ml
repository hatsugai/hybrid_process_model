type expr =
  Unit
| Bool of bool
| Int of int
| Var of Id.t
| Ref of expr
| Deref of expr
| And of expr * expr
| Or of expr * expr
| Tuple of expr list
| IfExpr of expr * expr * expr
| CaseExpr of expr * (Id.t * (Id.t list * expr)) list
| Fun of Id.t * Id.t list * stmt
| Apply of expr * expr list
| RecordRef of expr * Id.t
| RecordCons of (Id.t * expr) list
| RecordUpdate of expr * (Id.t * expr) list
[@@deriving show { with_path=false }, eq, ord]

and stmt =
  Skip
| Assign of Id.t * expr
| RAssign of expr * expr
| Let of Id.t * expr
| Detuple of Id.t list * expr
| Seq of stmt list
| If of expr * stmt * stmt
| Case of expr * (Id.t * (Id.t list * stmt)) list
| While of expr * stmt
| Return of expr
| Break
| Continue
| Select of syncterm list
[@@deriving show { with_path=false }, eq, ord]

and syncterm =
  Event of expr * stmt
| Receive of expr * Id.t * stmt
[@@deriving show { with_path=false }, eq, ord]

type definition =
  Constant of Id.t * expr
| Variable of Id.t * expr
| Function of Id.t * Id.t list * stmt

