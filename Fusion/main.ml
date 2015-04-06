			(** LECTURE DU FICHIER ET LANCEMENT DE L'ALGORITHME **)



open Cnf
open Parameters


(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)
					
					
(* Interprète le fichier *)
let parse file = Parser.cnf Lexer.token (lexbuf file)
					
					
(* Crée la CNF représentée dans le fichier *)
let create file =
	try
	parse file
	with _ -> (failwith "Erreur de saisie")


(* Fonction principale *)
let _ =
	
	(* Gestion des arguments et des options *)
	let file = ref "" in		(* Nom du fichier d'entrée *)
	let wl = ref false in
	let learning = ref false in	(* True ssi le clause learning est activé *)
	let draw = ref false in		(* True ssi le mode interactif est activé *)
	let unsat = ref false in	(* True ssi on veut prouver l'insatisfiabilité de la CNF *)
	let print = ref false in	(* True ssi on active l'affichage *)
	
	let options = [
		("-wl", Arg.Set wl, "Active les littéraux surveillés.") ;
		("-cl", Arg.Set learning, "Active l'apprentissage de clauses.") ;
		("-cl-interac", Arg.Unit (fun () -> learning := true ; draw := true), "Active l'apprentissage de clauses et le mode interactif.") ;
		("-explainunsat", Arg.Unit (fun () -> learning := true ; unsat := true), "Active la preuve de l'insatisfiabilité.") ;
		("-print", Arg.Set print, "Active l'affichage des étapes intermédiaires de l'algorithme.") ;
	] in
	
	Arg.parse options (fun s -> file := s)	"Ce programme résout l'instance de SAT donnée dans le fichier en entrée." ;
	
	(* Récupère la CNF à analyser *)
	let cnf = create !file in
	
	(* Affiche les warnings sur le nombre de clauses et de varaibles *)
	if cnf.v_real <> cnf.v then
		Printf.printf "Attention : L'indice maximal des variables est %d, alors que le nombre annoncé était %d\n" cnf.v_real cnf.v ;
	if cnf.c_real <> cnf.c then
		Printf.printf "Attention : Le fichier comporte %d clauses, alors que %d clauses étaient annoncées\n" cnf.c_real cnf.c ;
	
	let module E = (val (version !wl) : Clauses) in
	let module S = Dpll.Solve (E) in
	let res = S.solve cnf !wl !learning !draw !unsat !print in
	print_solution res
