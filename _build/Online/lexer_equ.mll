{
open Parser_equ ;;
}

let int = ['0'-'9']+


rule token = parse
	| [' ' '\t' '\n' ]	{ token lexbuf }		(* Saut des blancs/commentaires*)
	| "~"			{ NOT }
	| "("			{ LPAREN }
	| ")"			{ RPAREN }
	| "\\/"			{ OR }
	| "/\\"			{ AND }
	| "=>"			{ IMPLY }
	| "x" (int as s)	{ VAR (int_of_string s) }	(* Littéral *)
	| "="			{ EQU }
	| "!="			{ DIS }
	| eof			{ EOF }				(* Fin de fichier *)
