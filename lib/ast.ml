(* open Complex *)

(* type definitions for abstract syntax tree *)

type ident = Ident of string

type typ =
  | ConstType of ident
  | Lolli of typ * typ

type pattern = ident list

type term =
  | IdentTerm of ident
  | Lambda of ident * typ * term
  | App of term * term
  | LinComb of (Complex.t * term) list
  | Match of term * typ * (pattern * term) list

type command =
  | AssumCom of ident * typ
  | DefCom of ident * typ * term
  | IndDefCom of ident * ((ident * typ) list)

