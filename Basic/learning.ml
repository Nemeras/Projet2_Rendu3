			(** CLAUSE LEARNING **)

(* Implémente l'apprentissage de clause, le backtrack intelligent, la construction du graphe des conflits et le mode interactif *)

open Parameters
open DynArray
open Dot



		(** APPRENTISSAGE DE CLAUSE *)


(* S'il existe un seul littéral lit mis à faux au niveau de décision level dans c, renvoie true, lit.
   Sinon, renvoie false, lit où lit est le littréal mis à faux le plus récemment.                     *)
let only_one_blue c level solution levels orders =
	let rec aux c lit found =
		match c with
		| [] -> found <= 1, lit
		| x::q when x*solution.(abs x) < 0 && levels.(abs x) = level ->
			if found = 0 then
				aux q x 1
			else if orders.(abs lit) > orders.(abs x) then
				aux q lit (found + 1)
			else
				aux q x (found + 1)
		| _::q ->
			aux q lit found
	in
	aux c 0 0


(* Renvoie la clause à apprendre lorsqu'une contradiction est trouvée, ainsi que l'UIP correspondant.
   Place les couleurs jaune et violet sur le graphe, si nécessaire *)
let iter_learning graph clauses solution levels orders start activate para origins =
	
	let pos_c = ref start in		(* Position de la clause avec laquelle on va faire une résolution *)
	let c = ref clauses.a.(!pos_c) in	(* Clause sur laquelle on fait les résolutions *)
	
	let a, b = only_one_blue clauses.a.(!pos_c) para.level solution levels orders in
	let fini = ref a in			(* Indique si il ne reste plus qu'un seul littéral "bleu" dans c *)
	let lit = ref b in			(* Littéral sur lequel on fait la résolution / UIP *)
	
	(* Enregistrement des clauses servant aux résolutions *)
	if para.unsat then 
		begin
		DynArray.add origins [] [] ;
		origins.a.(origins.length - 1) <- (!pos_c)::(origins.a.(origins.length - 1));
		end
	;
	
	(* Tant qu'il reste plus d'un littéral mis à faux au niveau level dans c *)
	while (not !fini) do
		
		if activate then
			Dot.set_color graph (- !lit) Purple (Array.length solution - 1) ;
		
		pos_c := (abs solution.(abs !lit)) - 2 ;	(* Position de la clause ayant causé la mise à faux de lit *)
		(* Résolution : Fusion des deux clauses dans lesquelles on a enlevé la variable sur laquelle on fait la résolution *)
		c := Cnf.fusion (List.filter (fun i -> i <> !lit) !c) (List.filter (fun i -> i <> - !lit) clauses.a.(!pos_c)) ;
		
		if para.unsat then
			origins.a.(origins.length-1) <- (!pos_c)::(origins.a.(origins.length-1));
		
		let a, b = only_one_blue !c para.level solution levels orders in
		fini := a ;
		lit := b
		
	done ;
	
	(* L'UIP est en jaune *)
	if activate then
		Dot.set_color graph (- !lit) Yellow (Array.length solution - 1) ;
	!c, !lit




		(** CONSTRUCTION DU GRAPHE DU CONFLIT **)


(* Construit l'ensemble des arêtes du graphe, et place les couleurs bleu et blanc.
	start : noeud duquel on provient.
	seen : indique, pour chaque variable, si le noeud qui lui correspond a été traité ;
		permet d'éviter des doublons d'arêtes.
	v : nombre total de variables.                                                     *)
let rec edges pos_c graph current solution levels seen start level v =
	
	(* Parcourt tous les littéraux de l, qui sont "blancs". *)
	let rec explore_white l start =
		match l with
		| [] -> ()
		| n::q ->
			if not seen.(abs start) then
				begin
				Dot.add_edge graph (-n, start) ;
				Dot.set_color graph (-n) White v ;
				explore_white q start
				end
	in
	
	(* On parcourt les littéraux mis à faux de la clause pos_c *)
	match current.a.(pos_c) with

	| a, b, n::q when levels.(abs n) = level ->	(* Tant qu'on est sur le niveau de décision courant *)
		(* Ajout de -n au graphe *)
		Dot.add_edge graph (-n, start) ;
		Dot.set_color graph (-n) Blue v ;
		current.a.(pos_c) <- a,b,q ;
		
		(* Si -n n'est pas un pari, on peut continuer à explorer les littéraux qui ont causé la mise à faux de n *)
		if abs solution.(abs n) > 1 then
			 edges ((abs solution.(abs n)) - 2) graph current solution levels seen (-n) level v ;
		edges pos_c graph current solution levels seen start level v

	| a, b, q ->				(* Tous les autres littéraux sont "blancs" *)
		explore_white q start ;
		seen.(abs start) <- true



(* Construit le graphe du conflit.
   activate : autorise la construction du graphe. *)
let build_graph current solution levels level activate =
	let v =
		if activate then
			Array.length solution - 1
		else
			0
	in
	let graph = Dot.create_graph v in
	if activate then
		begin
		(* seen.(k) indique si la variable k a été explorée dans edges *)
		let seen = Array.make (Array.length solution) false in
		edges (-solution.(0)-1) graph current solution levels seen 0 level v ;
		end ;
	graph




		(** CLAUSE LEARNING ET BACKTRACK INTELLIGENT **)


(* Renvoie le niveau de décision maximal des littéraux de clause, hormis celui de l'UIP *)
let rec level_back clause levels uip =
	match clause with
	| [] -> -1
	| h::t when abs h <> abs uip -> max levels.(abs h) (level_back t levels uip)
	| h::t -> level_back t levels uip



(* Lors de la découverte d'une contradiction, gère l'apprentissage de clause et le mode interactif. *)
let learning stack clauses current pos solution levels orders k para origins =
	
	if para.learning then
		begin
		
		(* Mode interactif *)
		let activate = ref false in	(* Indique si, sur ce conflit, on active la construction du graphe *)
		if para.draw then
			begin
			print_string "\nConflit détecté. Que voulez-vous faire ?\ng : générer le graphe des confits\nc : continuer jusqu'au prochain conflit\nt : désactiver le mode interactif\n\n" ;
			flush stdout ;
			try
				let key = Scanf.scanf "%c\n" (fun x -> x) in
				match key with
				| 'g' -> activate := true
				| 't' -> para.draw <- false
				| 'c' -> ()
				| _ -> raise Exit
			with _ -> failwith "Il faut saisir un caractère valide"
			end
		;
		
		(* Construction de la nouvelle clause et du graphe *)
		let graph = build_graph current solution levels para.level !activate in
		let new_clause, uip = iter_learning graph clauses solution levels orders (-solution.(0)-1) !activate para origins in
		if !activate then
			Dot.compile graph (Array.length solution - 1)
		;
		
		Print_step.print_learning new_clause para.print ;
		
		(* Apprentissage de new_clause *)
		let clause_mod = Stack.maj_clause_learning stack new_clause pos levels clauses.length in
		DynArray.add clauses new_clause [] ;
		DynArray.add current clause_mod (false,[],[]) ;
		
		(* Détermination du niveau de backtrack *)
		let x = level_back new_clause levels uip in
		if x = -1 then
			(* new_clause est unitaire : on doit remonter jusqu'au début de la pile *)
			Stack.from_scratch stack current pos solution levels k para
		else
			para.nb_back <- para.level + 1 - x ;
		
		
		if para.draw then
			begin
			print_string (Cnf.string_of_clause new_clause) ;
			print_int x ; print_newline ()
			end
		end
