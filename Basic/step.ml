			(** ETAPES DE L'ALGORITHME **)

(* On donne ici les implémentations des fonctions utilisées dans l'algorithme, dans dpll.ml *)


open Array

open Parameters
open DynArray
open Stack
open Print_step




		(** BOOLEAN CONSTRAINT PROPAGATION **)


(* Cherche  un littéral présent dans une clause unitaire non encore vraie.
   Renvoie 0 s'il y a une contradiction, ou si un tel littéral n'existe pas.
   Sinon, renvoie un tel littéral et l'indice de la clause unitaire associée. *)
let find_consequences current solution =
	if solution.(0) = 0 then
		let i = ref 0 in
		let lit = ref 0 in
		let found = ref false in
		while not !found && !i < current.length do
			let b, c, _ = current.a.(!i) in
			if not b && List.tl c = [] then
				begin
				found := true ;
				lit := List.hd c
				end
			else
				incr i
		done ;
		if !found then 
			!lit, !i
		else
			0, 0
	else
		0, 0


(* Effectue la boolean constraint propagation sur toutes les variables présentes sous une seule polarité
   ou présentes dans une clause unitaire.                                                                *)
let rec propa stack current pos solution levels orders para =
	
	let level = para.level in
	let print = para.print in
	
	let a, b = find_consequences current solution in
	let x = ref a in
	let y = ref b in
	
	(* Détermine la numérotation de départ à placer dans orders *)
	let num = ref 1 in
	if para.learning then
		for i = 1 to Array.length solution - 1 do
			if solution.(i) != 0 && levels.(i) = level && orders.(i)+1 > !num then
				num := orders.(i) + 1 ;
		done
	;
	
	while !x <> 0 do
		if solution.(0) < 0 then
			x := 0
		else
			begin
			print_conseq !x print ;
			if levels.(abs !x) != -1 then
				levels.(abs !x) <- level ;
			if !x > 0 then
				(* x est nécessairement à vrai *)
				begin
				solution.(!x) <- !y + 2 ;
				update !x stack current pos solution (snd pos.(!x)) (fst pos.(!x))
				end
			else
				(* x est nécessairement à faux *)
				begin
				solution.(- !x) <- - !y - 2 ;
				update !x stack current pos solution (fst pos.(- !x)) (snd pos.(- !x))
				end
			;
			orders.(abs !x) <- !num ;
			incr num ;
			let a, b = find_consequences current solution in
			x := a ;
			y := b
			end
	done




		(** PARIS ET BACKTRACKING **)



(* Effectue une étape de backtrack *)
let backtrack_step stack current pos solution levels orders k para =
	
	(* Si la valeur de début de pile est positive et n'est pas issue d'une boolean constraint propagation,
	   alors l'opposé sera une déduction avec le clause learning ; sinon, on suppose l'opposé de cette valeur.
	   On arrête alors le backtrack.                                                                           *)
	if para.nb_back = 0 && !k > 0 && solution.(!k) = 1 then
		begin
		solution.(0) <- 0 ;
		para.back <- false ;
		k := backtrack stack current pos levels (snd pos.(!k)) para.level ;	(* On retire !k *)
		solution.(!k) <- 0 ;
		para.level <- para.level - 1 ;
		print_backtrack !k solution.(abs !k) para.print ;
		if not para.learning then
			begin
			k := - !k ;
			para.level <- para.level + 1 ;
			levels.(- !k) <- para.level ;
			update !k stack current pos solution (fst pos.(- !k)) (snd pos.(- !k)) ;	(* On suppose l'opposé *)
			print_hyp !k para.print ;
			solution.(- !k) <- -1 ;
			orders.(- !k) <- 0
			end
		end
	
	(* Sinon, il faut continuer le backtrack *)
	else
		begin
		if !k > 0 then
			k := backtrack stack current pos levels (snd pos.(!k)) para.level
		else
			k := backtrack stack current pos levels (fst pos.(- !k)) para.level
		;
		if abs solution.(abs !k) = 1 then
			para.level <- para.level - 1 ; (* Un niveau de décision a été entièrement annulé *)
		print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		k := pick stack
		end



(* Implémente une itération de la boucle *)
let continue stack clauses current pos solution levels orders k para origins =
	
	(* On vient de découvrir la clause vide : on commence le backtrack *)
	if solution.(0) < 0 && not para.back then
		begin
		
		if !k != 0 then
			k := pick stack ;	(* On a besoin de connaître la valeur à dépiler *)
		print_new_backtrack para.print ;
		para.back <- true ;
		
		(* Apprentissage de clause *)
		Learning.learning stack clauses current pos solution levels orders k para origins
		
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
			levels.(!k) <- para.level ;
			print_hyp !k para.print ;
			update !k stack current pos solution (snd pos.(!k)) (fst pos.(!k)) ;
			solution.(!k) <- 1 ;
			orders.(!k) <- 0
			end ;
		k := abs !k + 1 ;
		end
