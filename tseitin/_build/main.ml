			(** LECTURE DU FICHIER ET LANCEMENT DE L'ALGORITHME **)



open Form



(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)
					
					
(* Interprète le fichier *)
let parse file = New_parser.formula New_lexer.token (lexbuf file)
					
					
(* Crée la CNF représentée dans le fichier *)
let create file =
	try
	parse file
	with _ -> (failwith "Erreur de saisie")


(* Fonction principale *)
let _ =
	
	(* Gestion des arguments et des options *)
	let file = ref "" in		(* Nom du fichier d'entrée *)
	let learning = ref false in	(* True ssi le clause learning est activé *)
	let draw = ref false in		(* True ssi le mode interactif est activé *)
	let unsat = ref false in	(* True ssi on veut prouver l'insatisfiabilité de la CNF *)
	let print = ref false in	(* True ssi on active l'affichage *)

	let options = [
		("-print", Arg.Set print, "Active l'affichage des étapes intermédiaires de l'algorithme.") ;
		("-cl", Arg.Set learning, "Active l'apprentissage de clauses.") ;
		("-cl-interac", Arg.Unit (fun () -> learning := true ; draw := true), "Active l'apprentissage de clauses et le mode interactif.") ;
		("-explainunsat", Arg.Unit (fun () -> learning := true ; unsat := true), "Active la preuve de l'insatisfiabilité.")
	] in
	
	Arg.parse options (fun s -> file := s)	"Ce programme résout l'instance de SAT donnée dans le fichier en entrée." ;
	let tmp_tree,v_max = (parse !file) in
	let ftree = Tseitin.numeroter tmp_tree v_max in
	Tseitin.conv_tseitin ftree;;
