%{
open Ast
open Complex
%}

%token <Ast.ident> IDENT
%token <int> INTEGER
%token LPAREN RPAREN
%token ROOTOFUNITY SQRT
%token PLUS MINUS DIV
%token LAMBDA MAPSTO
%token MATCH RETURN WITH CASE END
%token COLON DEFEQUAL
%token TYPEDEF TERMDEF ASSUMPTION EOC
%token LOLLI


%left DIV
%right LOLLI

%start main
%type <Ast.command> main

%%
main:
  | TYPEDEF IDENT DEFEQUAL
      ind_constructors_with_EOC               { IndDefCom ($2, $4) }
  | TERMDEF IDENT COLON tp DEFEQUAL
      tm EOC                                  { DefCom ($2, $4, $6) }
  | ASSUMPTION IDENT COLON tp EOC             { AssumCom ($2, $4) }
;
ind_constructors_with_EOC:
  | CASE IDENT COLON tp more_ind_constructors { ($2, $4) :: $5 }
  | IDENT COLON tp more_ind_constructors      { ($1, $3) :: $4 }
  | EOC                                       { [] }
;
more_ind_constructors:
  | CASE IDENT COLON tp more_ind_constructors { ($2, $4) :: $5 }
  | EOC                                       { [] }
;
tp:
  | IDENT                                     { ConstType $1 }
  | LPAREN tp RPAREN                          { $2 }
  | tp LOLLI tp                               { Lolli ($1, $3) }
;
tm:
  | IDENT                                     { IdentTerm $1 }
  | LPAREN tm RPAREN                          { $2 }
  | tm tm                                     { App ($1, $2) }
  | LAMBDA IDENT COLON tp MAPSTO tm           { Lambda ($2, $4, $6) }
  | MATCH tm RETURN tp WITH match_cases       { Match ($2, $4, $6) }
  | tms_with_coeffs                           { LinComb $1 }
;
tms_with_coeffs:
  | tm_with_coeff                             { [$1] }
  | tm_with_coeff PLUS tms_with_coeffs        { $1 :: $3 }
  | tm_with_coeff MINUS tms_with_coeffs       {
      match $3 with
      | [] -> [$1]
      | (z, t) :: l -> $1 :: (neg z, t) :: l
  }
tm_with_coeff:
  | cmplx IDENT                               { ($1, IdentTerm $2) }
  | cmplx LPAREN tm RPAREN                    { ($1, $3) }
match_cases:
  | END                                       { [] }
  | pattern tm more_match_cases               { ($1, $2) :: $3 }
  | CASE pattern tm more_match_cases          { ($2, $3) :: $4 }
;
more_match_cases:
  | END                                       { [] }
  | CASE pattern tm more_match_cases          { ($2, $3) :: $4 }
;
pattern:
  | IDENT MAPSTO                              { [ $1 ] }
  | IDENT pattern                             { $1 :: $2 }
;
cmplx:
  | ROOTOFUNITY LPAREN INTEGER RPAREN         { exp {re=0.; im=2. *. Float.pi *. (Float.of_int $3)} }
  | SQRT LPAREN INTEGER RPAREN                { sqrt {re=Float.of_int $3; im=0.} }
  | INTEGER                                   { {re=Float.of_int $1; im=0.} }
  | cmplx DIV cmplx                           { div $1 $3 }



