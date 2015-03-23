			(** GESTION DE LA PILE **)



open List

open Parameters
open DynArray


		(** STRUCTURE DE PILE **)

(* Chaque étage de la pile contient :
	* Un entier indiquant le pari / la déduction effectuée (le littéral qui a été mis à vrai).
	* Une liste d'entiers indiquant quelles clauses sont mises à vrai lorsque ce littéral l'est.
   La fin de la pile est toujours 0, [].                                                              *)

type stack = (int*(int list)) list ref

let create_stack () =
	ref [(0,[])]

let is_empty stack =
	match !stack with
	| [] -> true
	| _ -> false

(* Renvoie l'élément de tête de la liste. *)
let pick stack =
	let k, _ = hd !stack in
	k



		(** ACTIVATION / DESACTIVATION DE CLAUSES **)


(* Réactualise pos lorsque c est activée = indéterminée. *)
let activate_clause c pos i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			pos.(x) <- i::(fst pos.(x)), snd pos.(x) ;
			aux q
		| x::q ->
			pos.(-x) <- fst pos.(-x), i::(snd pos.(-x)) ;
			aux q
	in
	aux c


(* Réactualise pos lorsque c est désactivée = vraie. *)
let inactivate_clause c pos i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			pos.(x) <- filter (fun y -> y <> i) (fst pos.(x)), snd pos.(x) ;
			aux q
		| x::q ->
			pos.(-x) <- fst pos.(-x), filter (fun y -> y <> i) (snd pos.(-x)) ;
			aux q
	in
	aux c




		(** UPDATE / PUSH **)


(* Supprime les littéraux mis à faux par l'affectation encours dans les clauses correspondantes. *)
let rec update_remove n stack current solution list_pos =
	match list_pos with
	| [] -> ()
	| h::t ->
		let boole, c, c2 = current.a.(h) in
		let new_c = List.filter (fun i -> i <> n) c in
		current.a.(h) <- boole, new_c, n::c2 ;
		if new_c = [] then
			solution.(0) <- -h-1 ;
		update_remove n stack current solution t


(* Désactive les clauses mises à vrai par l'affectation en cours. *)
let rec update_inactivate n stack current pos list_pos =
	match list_pos with
	| [] -> [] ;
	| h::t ->
		let boole, c, c2 = current.a.(h) in
		if not boole then
			begin
			current.a.(h) <- true, c, c2 ;
			inactivate_clause c pos h
			end
		;
		h::(update_inactivate n stack current pos t)


(* Place l'affectation n = vrai au début de la pile, et met à jour current et pos.
	list_pos_negative : liste des positions dans current des clauses contenant le littéral -n.
	list_pos : liste des positions dans current des clauses contenant le littéral n.           *)
let update n stack current pos solution list_pos_negative list_pos =
	let changes = update_inactivate n stack current pos list_pos in
	update_remove (-n) stack current solution list_pos_negative ;
	stack := (n, changes)::!stack




		(** BACKTRACK / POP **)


(* Réactive les clauses qui avaient été désactivées par l'affectation annulée. *)
let rec backtrack_activate n changes current pos =
	match changes with
	| [] -> ()
	| h::t ->
		let _, c, c2 = current.a.(h) in
		current.a.(h) <- false, c, c2 ;
		activate_clause c pos h ;
		backtrack_activate n t current pos


(* Enlève les littéraux mis à faux au niveau de décision level dans c2 *)
let rec aux_restore c2 levels level =
	match c2 with
	| [] -> []
	| n::_ when levels.(abs n) < level -> c2
	| n::q ->
		aux_restore q levels level

(* Restaure les littéraux qui avaient été supprimés par l'affectation annulée. *)
let rec backtrack_restore n to_restore current levels level =
	match to_restore with
	| [] -> ()
	| h::tail ->
		let boole, c, c2 = current.a.(h) in
		let new_c2 = aux_restore c2 levels level in
		current.a.(h) <- boole, n::c, new_c2 ;
		backtrack_restore n tail current levels level


(* Annule l'affectation en tête de liste, la renvoie, et met à jour current et pos. *)
let backtrack stack current pos levels to_restore level =
	let content = !stack in
	match content with
	| [] -> failwith "Pile vide"
	| (n, changes)::tail ->
		backtrack_activate n changes current pos ;
		stack := tail ;
		backtrack_restore (-n) to_restore current levels level ;
		n



		(** MANIPULATION DE LA PILE AVEC CLAUSE LEARNING **)


(* Met à jour pos avec les littéraux de clause, et constuit list_false en respectant l'ordre invers des affectations *)
let rec separate_aux clause list_false v pos pos_c=
	if List.mem (-v) !clause then 
		begin
		if v > 0 then
			pos.(v) <- fst pos.(v), pos_c::(snd pos.(v))
		else
			pos.(-v) <- pos_c::(fst pos.(-v)), snd pos.(-v)
		;
		list_false:= -v :: !list_false ;
		clause := List.filter (fun i -> i <> -v) (!clause)
		end


(* Remonte la pile et fait toutes les opérations correspondantes sur clause et pos *)
let rec separate stack clause list_false pos pos_c =
	match stack with
	| [] -> ()
	| (v, _)::tail ->
		separate_aux clause list_false v pos pos_c ;
		separate tail clause list_false pos pos_c


(* Traite la clause "clause" comme si elle avait été présente depuis le début de l'exécution dans current *)
let maj_clause_learning stack clause pos levels pos_c =
	let stack_rev = List.rev !stack in	(* On va remonter toute la pile d'affectations *)
	let new_clause = ref clause in	
	let list_false = ref [] in		(* Liste ordonnée des littéraux faux de !clause *)
	separate stack_rev new_clause list_false pos pos_c ;
	false, [], !list_false



(* Dans le cas où l'on doit remonter jusqu'au début de l'exécution, vide la pile et signifie à la boucle
   principale qu'elle ne doit pas s'arrêter.                                                               *)
let from_scratch stack current pos solution levels k para =
	(* Si k est nul, alors la contradiction est obtenue sans faire de paris, on ne fait donc rien pour
	   continuer l'algorithme car on sait que la CNF n'est pas satisfiable.                            *)
	if (!k != 0) then
		begin
		while !k != 0 do
			if !k > 0 then
				k := backtrack stack current pos levels (snd pos.(!k)) para.level
			else
				k := backtrack stack current pos levels (fst pos.(- !k)) para.level
			;
			if abs solution.(abs !k) = 1 then
				para.level <- para.level - 1 ;
			solution.(abs !k) <- 0 ;
			k := pick stack
		done ;
		
		(* On place k à 0 pour que l'agorithme ne s'arrête pas encore, et solution.(0) à 1 pour
		   indiquer à la boucle principale de placer k à 0.                                       *)
		k := 1 ;
		solution.(0) <- 1
		end
