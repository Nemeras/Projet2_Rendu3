{
open Parser;;
}

let int = ['0'-'9']+			(* Un entier positif ou n�gatif *)
let comment = 'c' [^ '\n']* ('\n' | eof)	(* Un commentaire est d�limit� par c et un \n ou un eof *)
let space = [' ' '\t']+				(* Un espace entre deux cha�nes de caract�res signifiantes *)

rule token = parse
	  | [' ' '\t' '\n']			        { token lexbuf }	(* Saut des blancs/commentaires*)
          | "~"                                         { NOT }
          | "("                                         { LPAREN }
          | ")"                                         { RPAREN }
          | "\\/"                                       { OR }
          | "/\\"                                       { AND }
          | "=>"                                        { IMPLY }
	  | int as s					{ LIT (int_of_string s) }			(* Litt�ral *)
	  | eof						{ EOF }						(* Fin de fichier *)
