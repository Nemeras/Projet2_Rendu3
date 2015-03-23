			(** ETAPES DE L'ALGORITHME **)

(* On donne ici les implémentations des fonctions utilisées dans l'algorithme, dans dpll.ml *)


open Parameters
open DynArray
open Stack
open Print_step




		(** BOOLEAN CONSTRAINT PROPAGATION **)


(* Trouve toutes les conséquences des clauses unitaires (units) apparues à cette étape *)
let rec propa units stack current solution levels orders level print =
	
	let rec aux units num =
		match units with
		| [] -> ()
		| _ when solution.(0) < 0 -> ()
		| (x,i)::tail when solution.(abs x) = 0 ->
			if x > 0 then
				solution.(x) <- i + 2
			else
				solution.(-x) <- -i - 2
			;
			levels.(abs x) <- level ;
			orders.(abs x) <- num ;
			print_conseq x print ;
			let l = update x stack current solution in
			aux (l@tail) (num+1)
		| (x,i)::tail -> aux tail num
	in
	
	(* Détermine la numérotation de départ à placer dans orders *)
	let compt = ref 1 in
	for i = 1 to Array.length solution - 1 do
		if solution.(i) != 0 && levels.(i) = level && orders.(i)+1 > !compt then
			compt := orders.(i) + 1 ;
	done ;
	
	aux units !compt




		(** PARIS ET BACKTRACKING **)



(* Effectue une étape de backtrack *)
let backtrack_step stack current solution levels orders uni k para =
	
	(* Si la valeur de début de pile est positive et n'est pas issue d'une boolean constraint propagation,
	   donc pas nécessaire, on peut supposer l'opposé. On arrête alors le backtrack.                       *)
	if para.nb_back = 0 && !k > 0 && solution.(!k) = 1 then
		begin
		solution.(0) <- 0 ;
		para.back <- false ;
		k := backtrack stack ;
		print_backtrack !k solution.(abs !k) para.print ;
		solution.(!k) <- 0 ;
		levels.(!k) <- 0 ;
		para.level <- para.level - 1 ;
		if not para.learning then
			begin
			k := - !k ;
			para.level <- para.level + 1 ;
			solution.(- !k) <- -1 ;
			uni := update !k stack current solution ;
			print_hyp !k para.print ;
			levels.(- !k) <- para.level ;
			orders.(- !k) <- 0
			end
		end
	
	(* Sinon, il faut continuer le backtrack *)
	else
		begin
		if abs solution.(abs !k) = 1 then
			para.level <- para.level - 1 ; (* Un niveau de décision a été entièrement annulé *)
		print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		levels.(abs !k) <- 0 ;
		k := backtrack stack ;
		k := pick stack
		end




(* Implémente une itération de la boucle *)
let continue stack clauses current solution levels orders uni k para origins =
	
	(* On vient de découvrir la clause vide : on commence le backtrack *)
	if solution.(0) < 0 && not para.back then
		begin
		
		if !k != 0 then
			k := pick stack ;		(* On a besoin de connaître la valeur à dépiler *)
		print_new_backtrack para.print ;
		para.back <- true ;
		
		(* Apprentissage de clause *)
		Learning.learning stack clauses current solution levels orders k para origins
		
		end
	
	(* Backtracking : on n'a pas encore pu faire de nouvelle hypothèse pour enlever la contradiction *)
	else if para.back then
		begin
		(* On décrémente nb_back si on a passé un niveau de décision *)
		if abs solution.(abs !k) = 1 && para.nb_back > 0 then
			para.nb_back <- para.nb_back - 1 ;
		backtrack_step stack current solution levels orders uni k para
		end
	
	(* S'il n'y a pas de contradiction : on suppose par défaut la première variable libre comme vraie *)
	else
		begin
		if !k <> 0 && solution.(abs !k) = 0 then
			begin
			k := abs !k ;
			para.level <- para.level + 1 ;
			print_hyp !k para.print ;
			solution.(!k) <- 1 ;
			levels.(!k) <- para.level ;
			orders.(!k) <- 0 ;
			uni := update !k stack current solution ;
			end ;
		k := abs !k + 1 ;
		end
