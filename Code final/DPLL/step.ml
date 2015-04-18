			(** ETAPES DE L'ALGORITHME **)

open General
open Theories

(* On donne ici les implémentations des fonctions utilisées dans l'algorithme, dans dpll.ml *)


open Types
open Learning
open DynArray
open Print_step


module Step (C : Clauses) (T : Theory) =
struct


module P = Print_step (C)

		(** PARIS ET BACKTRACKING **)



(* Effectue une étape de backtrack *)
let backtrack_step stack solver current pos solution levels orders k para =
	
	(* Si la valeur de début de pile est positive et n'est pas issue d'une boolean constraint propagation,
	   donc pas nécessaire, on peut supposer l'opposé. On arrête alors le backtrack.                       *)
	if para.nb_back = 0 && (!k > 0 || para.learning) && abs solution.(abs !k) = 1 then
		begin
		solution.(0) <- 0 ;
		para.back <- false ;
		k := C.backtrack stack current pos solution ;
		T.backtrack solver !k ;
		if para.vsids then
			begin
			Heap.add_score !k para ;
			Heap.add_score (- !k) para
			end ;
		P.print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		levels.(abs !k) <- 0 ;
		para.level <- para.level - 1 ;
		if not para.learning then
			begin
			k := - !k ;
			para.level <- para.level + 1 ;
			solution.(- !k) <- -1 ;
			solution.(0) <- T.update solver !k ;
			let _ = C.update !k stack current pos solution in
			P.print_hyp !k para.print ;
			levels.(- !k) <- para.level ;
			orders.(- !k) <- 0
			end
		end
	
	(* Sinon, il faut continuer le backtrack *)
	else
		begin
		if abs solution.(abs !k) = 1 then
			para.level <- para.level - 1 ; (* Un niveau de décision a été entièrement annulé *)
		P.print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		levels.(abs !k) <- 0 ;
		T.backtrack solver !k ;
		k := C.backtrack stack current pos solution ;
		if para.vsids then
			begin
			Heap.add_score !k para ;
			Heap.add_score (- !k) para
			end ;
		k := C.pick stack
		end




(* Implémente une itération de la boucle *)
let continue stack solver clauses current pos origins solution levels orders k para =
	
	(* On vient de découvrir la clause vide : on commence le backtrack *)
	if solution.(0) < 0 && not para.back then
		begin
		
		if !k != 0 then
			k := C.pick stack ;		(* On a besoin de connaître la valeur à dépiler *)
		P.print_new_backtrack para.print ;
		para.back <- true ;
		
		if solution.(0) = -max_int then
			begin
			let new_clause = T.unsat solver in
			let clause_mod = C.maj_cl stack new_clause pos levels current.length in
			DynArray.add clauses new_clause [] ;
			DynArray.add current clause_mod (C.init_value [])
			end
		else
			(* Apprentissage de clause *)
			let module L = Learning (C) (T) in
			L.learning stack solver clauses current pos solution levels orders k para origins
		
		end
	
	(* Backtracking : on n'a pas encore pu faire de nouvelle hypothèse pour enlever la contradiction *)
	else if para.back then
		begin
		(* On décrémente nb_back si on a passé un niveau de décision *)
		if abs solution.(abs !k) = 1 && para.nb_back > 0 then
			para.nb_back <- para.nb_back - 1 ;
		backtrack_step stack solver current pos solution levels orders k para
		end
	
	(* S'il n'y a pas de contradiction : on suppose par défaut la première variable libre comme vraie *)
	else
		begin
		(* Si l'heuristique VSIDS est activée, on choisit le k suivant *)
		if para.vsids then
			k := Heap.next para ;
		if !k <> 0 && solution.(abs !k) = 0 then
			begin
			if not para.vsids then
				k := abs !k ;
			para.level <- para.level + 1 ;
			P.print_hyp !k para.print ;
			solution.(abs !k) <- (abs !k)/ !k ;
			solution.(0) <- T.update solver !k ;
			let _ = C.update !k stack current pos solution in
			if para.vsids then
				begin
				Heap.remove_score !k para ;
				Heap.remove_score (- !k) para
				end ;
			levels.(abs !k) <- para.level ;
			orders.(abs !k) <- 0
			end ;
		if not para.vsids then
			k := abs !k + 1
		end



end
