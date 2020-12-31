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
	| "return"			{ RETURN }
	| "with" 			{ WITH }
	| "|"				{ CASE }
	| "end"				{ END }
	| "Type"			{ TYPEDEF }
	| "Let"				{ TERMDEF }
	| "Parameter"		{ ASSUMPTION }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*['\'']* as id { IDENT(Ident id) }
	| eof				{ raise Eof }
