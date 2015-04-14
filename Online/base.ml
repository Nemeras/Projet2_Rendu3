type atom = int
type struc = int


(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)

(* Interprète le fichier *)
let parse file =
	Parser_cnf.cnf Lexer_cnf.token (lexbuf file)

(* Crée la CNF représentée dans le fichier *)
let create file =
	try
		let cnf = parse file in

		(* Affiche les warnings sur le nombre de clauses et de varaibles *)
		if cnf.v_real <> cnf.v then
			Printf.printf "Attention : L'indice maximal des variables est %d, alors que le nombre annoncé était %d\n" cnf.v_real cnf.v ;
		if cnf.c_real <> cnf.c then
			Printf.printf "Attention : Le fichier comporte %d clauses, alors que %d clauses étaient annoncées\n" cnf.c_real cnf.c ;
		
		cnf, cnf.v_real
	with _ -> (failwith "Erreur de saisie")

let update solver x =
	0

let backtrack solver x =
	()

let unsat solver =
	[0]

let print_solution res solver =
	Cnf.print_solution res solver
