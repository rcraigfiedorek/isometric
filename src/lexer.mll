{
open Parser
exception Eof
}

let whitespace = [' ' '\t' '\n']

rule token = parse
	| whitespace		{ token lexbuf }
	| "."				{ EOC }
	| "("				{ LPAREN }
	| ")"				{ RPAREN }
	| ":"				{ COLON }
	| ":="				{ DEFEQUAL }
	| "-o"				{ LOLLI }
	| "fun"				{ LAMBDA }
	| "=>"				{ MAPSTO }
	| "match" 			{ MATCH }
	| "with" 			{ WITH }
	| "|"				{ CASE }
	| "end"				{ END }
	| "Type"			{ TYPEDEF }
	| "Let"				{ TERMDEF }
	| "Parameter"		{ ASSUMPTION }
	| ['a'-'z']['a'-'z' 'A'-'Z' '_' '0'-'9']*['\'']* as id { VAR(Var id) }
	| ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*['\'']* as id { CONST(Const id) }
	| eof				{ raise Eof }
