%{
open Ast
%}


%token <Ast.var> VAR
%token <Ast.const> CONST
%token LPAREN RPAREN
%token LOLLI
%token LAMBDA MAPSTO
%token MATCH WITH CASE END
%token COLON DEFEQUAL
%token TYPEDEF TERMDEF ASSUMPTION EOC

%right LOLLI

%start main
%type <Ast.command> main

%%
main:
  | TYPEDEF CONST DEFEQUAL
      ind_constructors EOC                    { IndDefCom ($2, $4) }
  | TERMDEF CONST COLON tp DEFEQUAL
      tm EOC                                  { DefCom ($2, $4, $6) }
  | ASSUMPTION CONST COLON tp EOC             { AssumCom ($2, $4) }
;
ind_constructors:
  | END                                       { [] }
  | CASE CONST COLON tp ind_constructors      { ($2, $4) :: $5 }
;
tp:
  | CONST                                     { ConstType $1 }
  | LPAREN tp RPAREN                          { $2 }
  | tp LOLLI tp                               { Lolli ($1, $3) }
;
tm:
  | VAR                                       { VarTerm $1 }
  | CONST                                     { ConstTerm $1 }
  | LPAREN tm RPAREN                          { $2 }
  | tm tm                                     { App ($1, $2) }
  | LAMBDA VAR COLON tp MAPSTO tm             { Lambda ($2, $4, $6) }
  | MATCH tm COLON tp WITH match_cases        { Match ($2, $4, $6) }
      /*  This is a maaaajor problem. Match syntax doesn't make sense because we haven't
      made an interpreter yet */
;
match_cases:
  | END                                       { [] }
  | CASE tm match_cases                       { $2 :: $3 }
;



