			(** ALGORITHME DPLL **)

(* La structure principale de l'algorithme DPLL ; la pile est gérée dans stack.ml, les étapes intermédiaires dans step.ml *)


open List

open Parameters
open DynArray
open Cnf
open Step
open Stack
open Print_step



		(** STRUCTURE DE DONNEE - VARIABLES **)


type watched_clauses = clause dynarray


(* On liste ici uniquement les changements par rapport à la version sans littéraux surveillés :
	* pos n'est plus utilisée.
	* current n'est plus qu'un tableau dynamique de clause : ce seront les littéraux surveillés qui
	  indiqueront si la clause est activée ou pas.
	* uni : clauses unitaires rencontrées.
	  (x,i) est dans uni si x doit être mis à vrai par déduction sur la clause d'indice i.          *)





		(** INITIALISATION **)



(* Cherche les clauses unitaires dans current et met à jour uni *)
let units current solution uni =
	for i = 0 to current.length - 1 do
		if Watched.is_unit current.a.(i) solution then
			uni := (hd current.a.(i), i) :: !uni
	done


(* Renvoie le tableau current correspondant à la liste de clauses "clauses". *)
let cnf_to_vect cl =
	let clauses = DynArray.make (List.length cl) [] in
	let current = DynArray.make (List.length cl) [] in
	let rec aux cl i =
		match cl with
		| [] -> ()
		| c::tail ->
			current.a.(i) <- c ;
			clauses.a.(i) <- c ;
			aux tail (i+1)
	in
	aux cl 0 ;
	clauses, current





		(** RESOLUTION - STRUCTURE DE DPLL **)



(* Renvoie une solution associée à la CNF cnf donnée en entrée :
	False si cnf n'est pas satisfiable.
	True solution si cnf est satisfiable, avec solution une instanciation qui la satisfait. *)

let solve cnf learning draw unsat print =
	
	(* Tri des littéraux dans les clauses par indice de variable croissant,
	   élimination des tautologies.                                         *)
	ordo cnf ;
	
	(* Initialisation de current, solution et de la pile des instanciations.
           Elimination des clauses avec moins de un littéral.                    *)
	let solution = Array.make (cnf.v_real+1) 0 in
	let levels = Array.make (cnf.v_real+1) 0 in
	let orders = Array.make (cnf.v_real+1) 0 in
	let clauses, current = cnf_to_vect cnf.clauses in
	let l = current.length in		(* Nombre de clauses dans la CNF initiale *)
	let stack = create_stack () in		(* stack contient initialement 0 en fond de pile *)
	
	let origins =
		if unsat then
			DynArray.make clauses.length []
		else
			DynArray.make 0 []
	in
	
	
	let k = ref (
		if solution.(0) < 0 then	(* Si la clause vide est dans la CNF *)
			0
		else
			1
	) in
	
	let uni = ref [] in
	let compt = ref 0 in
	
	(* Paramètres des algorithmes de Step *)
	let para = {
		back = false ;
		nb_back = 0 ;
		level = -1 ;
		learning = learning ;
		draw = draw ;
		unsat = unsat ;
		print = print
	} in
	
	while abs !k <= cnf.v_real && !k <> 0 do
		incr compt ;
		
		(* Si on est revenu au début des paris car on a appris une clause unitaire, on réinitialise
		   les valeurs et on reprend l'exécution.                                                   *)
		if solution.(0) > 0 then
			begin
			solution.(0) <- 0 ;
			para.back <- false ;
			para.level <- -1 ;
			k := 0
			end
		;
		
		(* Affichage, si autorisé *)
		if print then
			print_step current solution para.back !compt ;
		
		(* Détection des clauses unitaires *)
		if solution.(0) = 0 then
			units current solution uni ;
		
		(* Boolean constraint propagation *)
		propa !uni stack current solution levels orders para.level para.print ;
		uni := [] ;
		
		(* Si toutes les variables ont été instanciées *)
		if abs !k = cnf.v_real then
			(* S'il y a contradiction : backtrack *)
			if solution.(0) < 0 then
				continue stack clauses current solution levels orders uni k para origins
			(* Sinon : c'est fini *)
			else
				k := cnf.v_real + 1
		(* Sinon : on continue *)
		else
			continue stack clauses current solution levels orders uni k para origins
	done ;
	
	if !k = 0 then
		begin
		(* Preuve de l'insatisfiabilité *)
		if para.unsat then
			Unsat.create_unsat origins clauses solution l ;
		False
		end
	else
		True solution
