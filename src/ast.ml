open Complex

(* type definitions for abstract syntax tree *)

type ident = Ident of string

type typ_expr =
  | ConstTypeExpr of ident
  | LolliExpr of typ_expr * typ_expr

type pattern_expr = ident list

type term_expr =
  | IdentTermExpr of ident
  | LambdaExpr of ident * typ_expr * term_expr
  | AppExpr of term_expr * term_expr
  | LinCombExpr of (Complex.t * term_expr) list
  | MatchExpr of term_expr * typ_expr * (pattern_expr * term_expr) list

type command_expr =
  | AssumCom of ident * typ_expr
  | DefCom of ident * typ_expr * term_expr
  | IndDefCom of ident * ((ident * typ_expr) list)

