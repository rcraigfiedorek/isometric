{
open Parser
exception Eof
}

let whitespace = [' ' '\t' '\n']

rule token = parse
	| whitespace		{ token lexbuf }
	| "."				  { EOC }
	| "("				  { LPAREN }
	| ")"				  { RPAREN }
	| ":"				  { COLON }
	| ":="				{ DEFEQUAL }
	| "-o"				{ LOLLI }
	| "fun"				{ LAMBDA }
	| "=>"				{ MAPSTO }
	| "match" 		{ MATCH }
	| "return"	  { RETURN }
	| "with" 			{ WITH }
	| "|"				  { CASE }
	| "end"				{ END }
	| "Type"			{ TYPEDEF }
	| "Let"				{ TERMDEF }
	| "Parameter" { ASSUMPTION }
	| "root"      { ROOTOFUNITY }
	| "+"         { PLUS }
	| "-"         { MINUS }
	| "sqrt"      { SQRT }
	| "/"         { DIV }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*['\'']* as id { IDENT(Ident id) }
	| ['0'-'9']+ as n { INTEGER(int_of_string n) }
	| eof				  { raise Eof }
(* TODO: add specific lexing errors. Parse all things that don't fit here to avoid empty token errors *)
