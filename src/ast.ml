open Complex

(* type definitions for abstract syntax tree *)

type var = Var of string

type const = Const of string

type typ = ConstType of const | Lolli of typ * typ

type term =
  | VarTerm of var
  | ConstTerm of const
  | Lambda of var * typ * term
  | App of term * term
  | LinComb of (Complex.t * term) list
  | Match of term * typ * term list

type local_decl = var * typ

type local_ctx = local_decl list

type global_decl =
  | GlobalAssum of const * typ
  | GlobalDef of const * term * typ
  | GlobalIndDef of const * ((const * typ) list)

type global_ctx = global_decl list

type command =
  | AssumCom of const * typ
  | DefCom of const * term * typ
  | IndDefCom of const * ((const * typ) list)

type file = command list