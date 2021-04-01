%{
open Ast
%}


%token <Ast.ident> IDENT
%token LPAREN RPAREN
%token LOLLI
%token LAMBDA MAPSTO
%token MATCH RETURN WITH CASE END
%token COLON DEFEQUAL
%token TYPEDEF TERMDEF ASSUMPTION EOC

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
  | IDENT                                     { ConstTypeExpr $1 }
  | LPAREN tp RPAREN                          { $2 }
  | tp LOLLI tp                               { LolliExpr ($1, $3) }
;
tm:
  | IDENT                                     { IdentTermExpr $1 }
  | LPAREN tm RPAREN                          { $2 }
  | tm tm                                     { AppExpr ($1, $2) }
  | LAMBDA IDENT COLON tp MAPSTO tm           { LambdaExpr ($2, $4, $6) }
  | MATCH tm RETURN tp WITH match_cases       { MatchExpr ($2, $4, $6) }
;
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



