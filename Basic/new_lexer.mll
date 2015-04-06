{
open Parser;;
}

let int = ['0'-'9']+			(* Un entier positif ou négatif *)
let comment = 'c' [^ '\n']* ('\n' | eof)	(* Un commentaire est délimité par c et un \n ou un eof *)
let space = [' ' '\t']+				(* Un espace entre deux chaînes de caractères signifiantes *)

rule token = parse
	  | [' ' '\t' '\n']			        { token lexbuf }	(* Saut des blancs/commentaires*)
          | "~"                                         { NOT }
          | "("                                         { LPAREN }
          | ")"                                         { RPAREN }
          | "\\/"                                       { OR }
          | "/\\"                                       { AND }
          | "=>"                                        { IMPLY }
	  | int as s					{ LIT (int_of_string s) }			(* Littéral *)
	  | eof						{ EOF }						(* Fin de fichier *)
