{
open New_parser;;
}

let int = ['0'-'9']+			(* Un entier positif ou négatif *)


rule token = parse
	  | [' ' '\t' '\n' ]				        { token lexbuf }	(* Saut des blancs/commentaires*)
          | "~"                                         { NOT }
          | "("                                         { LPAREN }
          | ")"                                         { RPAREN }
          | "\\/"                                       { OR }
          | "/\\"                                       { AND }
          | "=>"                                        { IMPLY }
	  | int as s					{ LIT (int_of_string s) }			(* Littéral *)

	  | eof						{ EOF }						(* Fin de fichier *)
