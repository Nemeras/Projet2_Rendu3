type atom = int
type struc = int


(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)

let lexstr s =
	Lexing.from_string s

let parse file =
	Parser_tseitin.formula Lexer_tseitin.token (lexbuf file)

let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)

let create file =
	try
		let f, m = parse file in
		let s = Tseitin.conv_tseitin f m in
		let cnf = parse_cnf s in
		cnf, m
	with _ -> (failwith "Erreur de saisie")


let update solver x =
	0

let backtrack solver x =
	()

let unsat solver =
	[0]

let print_solution res solver =
	Cnf.print_solution res solver
