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
%type <Ast.command_expr> main

%%
main:
  | TYPEDEF IDENT DEFEQUAL
      ind_constructors EOC                    { IndDefCom ($2, $4) }
  | TERMDEF IDENT COLON tp DEFEQUAL
      tm EOC                                  { DefCom ($2, $4, $6) }
  | ASSUMPTION IDENT COLON tp EOC             { AssumCom ($2, $4) }
;
ind_constructors:
  | END                                       { [] }
  | CASE IDENT COLON tp ind_constructors      { ($2, $4) :: $5 }
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
  | CASE pattern tm match_cases               { ($2, $3) :: $4 }
;
pattern:
  | IDENT MAPSTO                              { [ $1 ] }
  | IDENT pattern                             { $1 :: $2 }
;



