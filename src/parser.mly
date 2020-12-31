%{
open Ast
%}


%token <var> VAR
%token <const> CONST
%token LPAREN RPAREN
%token LOLLI
%token LAMBDA MAPSTO
%token MATCH WITH CASE END
%token COLON DEFEQUAL
%token TYPEDEF TERMDEF ASSUMPTION EOC

%right LOLLI

%start main
%type <command> main

%%
main:
  | TYPEDEF CONST DEFEQUAL
      ind_constructors EOC                    { IndDefCom ($2, $4) }
  | TERMDEF CONST COLON type DEFEQUAL
      term EOC                                { DefCom ($2, $4, $6) }
  | ASSUMPTION CONST COLON type EOC           { AssumCom ($2, $4) }

ind_constructors:
  | END                                       { [] }
  | CASE CONST COLON type ind_constructors    { ($2, $4) :: $5 }

type:
  | CONST                                     { ConstType $1 }
  | LPAREN type RPAREN                        { $2 }
  | type LOLLI type                           { Lolli ($1, $3) }

term:
  | VAR                                       { VarTerm $1 }
  | CONST                                     { ConstTerm $1 }
  | LPAREN term RPAREN                        { $2 }
  | term term                                 { App ($1, $2) }
  | LAMBDA VAR COLON type MAPSTO term         { Lambda ($2, $4, $6) }
  | MATCH term COLON type WITH match_cases    { Match ($2, $4, $6) }
      /*  This is a maaaajor problem. Match syntax doesn't make sense because we haven't
      made an interpreter yet */

match_cases:
  | END                                       { [] }
  | CASE term match_cases                     { $2 :: $3 }


%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
  | MINUS expr %prec UMINUS { - $2 }
;