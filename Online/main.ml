			(** LECTURE DU FICHIER ET LANCEMENT DE L'ALGORITHME **)



open Cnf
open Parameters


module Theory (T : Theory) =
struct

let init file wl learning draw unsat print =
	
	let cnf, solver = T.create file in
	
	let module E = (val (version wl) : Clauses) in
	let module S = Dpll.Solve (E) (T) in
	
	let res = S.solve cnf solver wl learning draw unsat print in
	T.print_solution res solver

end

(* Fonction principale *)
let _ =
	
	(* Gestion des arguments et des options *)
	let file = ref "" in		(* Nom du fichier d'entrée *)
	let wl = ref false in
	let learning = ref false in	(* True ssi le clause learning est activé *)
	let draw = ref false in		(* True ssi le mode interactif est activé *)
	let unsat = ref false in	(* True ssi on veut prouver l'insatisfiabilité de la CNF *)
	let print = ref false in	(* True ssi on active l'affichage *)
	
	let theory = ref 0 in
	
	let options = [
		("-wl", Arg.Set wl, "Active les littéraux surveillés.") ;
		("-cl", Arg.Set learning, "Active l'apprentissage de clauses.") ;
		("-cl-interac", Arg.Unit (fun () -> learning := true ; draw := true), "Active l'apprentissage de clauses et le mode interactif.") ;
		("-explainunsat", Arg.Unit (fun () -> learning := true ; unsat := true), "Active la preuve de l'insatisfiabilité.") ;
		("-print", Arg.Set print, "Active l'affichage des étapes intermédiaires de l'algorithme.") ;
		("-tseitin", Arg.Unit (fun () -> theory := 1), "Lit une formule logique quelconque.") ;
		("-equality", Arg.Unit (fun () -> theory := 2), "Solveur sur la théorie de l'égalité.")
	] in
	
	Arg.parse options (fun s -> file := s)	"Ce programme résout l'instance de SAT donnée dans le fichier en entrée." ;
	
	let module T = (val (choose_theory !theory) : Theory) in
	let module Launch = Theory (T) in
	Launch.init !file !wl !learning !draw !unsat !print
	
