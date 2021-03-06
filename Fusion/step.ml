			(** ETAPES DE L'ALGORITHME **)

(* On donne ici les implémentations des fonctions utilisées dans l'algorithme, dans dpll.ml *)


open Parameters
open Learning
open DynArray
open Stack
open Print_step

module Step (E : Clauses) =
struct


module P = Print_step.Print(E)

		(** PARIS ET BACKTRACKING **)



(* Effectue une étape de backtrack *)
let backtrack_step stack current pos solution levels orders k para =
	
	(* Si la valeur de début de pile est positive et n'est pas issue d'une boolean constraint propagation,
	   donc pas nécessaire, on peut supposer l'opposé. On arrête alors le backtrack.                       *)
	if para.nb_back = 0 && !k > 0 && solution.(!k) = 1 then
		begin
		solution.(0) <- 0 ;
		para.back <- false ;
		k := E.backtrack stack current pos solution ;
		P.print_backtrack !k solution.(abs !k) para.print ;
		solution.(!k) <- 0 ;
		levels.(!k) <- 0 ;
		para.level <- para.level - 1 ;
		if not para.learning then
			begin
			k := - !k ;
			para.level <- para.level + 1 ;
			solution.(- !k) <- -1 ;
			E.update !k stack current pos solution ;
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
		k := E.backtrack stack current pos solution ;
		k := E.pick stack
		end




(* Implémente une itération de la boucle *)
let continue stack clauses current pos origins solution levels orders k para =
	
	(* On vient de découvrir la clause vide : on commence le backtrack *)
	if solution.(0) < 0 && not para.back then
		begin
		
		if !k != 0 then
			k := E.pick stack ;		(* On a besoin de connaître la valeur à dépiler *)
		P.print_new_backtrack para.print ;
		para.back <- true ;
		
		(* Apprentissage de clause *)
		let module L = Learning_p(E) in
		L.learning stack clauses current pos solution levels orders k para origins
		
		end
	
	(* Backtracking : on n'a pas encore pu faire de nouvelle hypothèse pour enlever la contradiction *)
	else if para.back then
		begin
		(* On décrémente nb_back si on a passé un niveau de décision *)
		if abs solution.(abs !k) = 1 && para.nb_back > 0 then
			para.nb_back <- para.nb_back - 1 ;
		backtrack_step stack current pos solution levels orders k para
		end
	
	(* S'il n'y a pas de contradiction : on suppose par défaut la première variable libre comme vraie *)
	else
		begin
		if !k <> 0 && solution.(abs !k) = 0 then
			begin
			k := abs !k ;
			para.level <- para.level + 1 ;
			P.print_hyp !k para.print ;
			solution.(!k) <- 1 ;
			E.update !k stack current pos solution ;
			levels.(!k) <- para.level ;
			orders.(!k) <- 0
			end ;
		k := abs !k + 1 ;
		end



end
