			(** GESTION DE LA PILE **)

(* Remarque : les opérations sur les clauses sont gérées dans watched.ml *)


open List

open Parameters
open Watched
open DynArray


		(** STRUCTURE DE PILE **)

(* Chaque étage de la pile contient un entier indiquant le pari / la déduction effectuée
   (le littéral qui a été mis à vrai). La fin de la pile est toujours 0.                 *)

type stack = int list ref

let create_stack () =
	ref [0]

let is_empty liste =
	match !liste with
	| [] -> true
	| _ -> false

(* Renvoie l'élément de tête de la liste. *)
let pick stack =
	hd !stack




		(** UPDATE / PUSH **)


(* Place l'affectation n = vrai au début de la pile, renvoie la liste des conséquences (clauses unitaires) apparues. *)
let update n stack current solution =
	let consequences = ref [] in
	stack := n::!stack ;
	let absurd = ref false in
	let i = ref 0 in
	while (!i < current.length && not !absurd)  do
		(* Si un des deux littéraux de la clause n'est pas encore à vrai *)
		if not (is_w_true current.a.(!i) solution) then
			begin
			
			(* On modifie les littéraux surveillés suite à l'affectation en cours *)
			current.a.(!i) <- change_clause current.a.(!i) solution ;
			
			(* Détection d'une conséquence *)
			if is_unit current.a.(!i) solution then
				consequences := (hd current.a.(!i), !i)::!consequences ;
			
			(* Si la clause est à faux, il y a contradiction *)
			is_clause_false current.a.(!i) solution ;
			if solution.(0) < 0 then
				begin
				solution.(0) <- -1 - !i ;
				absurd := true
				end
			end
		;
		
		(* Sinon, on ignore cette clause *)
		
		incr i
		
	done ;
	
	!consequences




		(** BACKTRACK / POP **)


(* Annule l'affectation en tête de liste et la renvoie. *)
let backtrack stack =
	let n = hd !stack in
	stack := tl !stack ;
	n




		(** MANIPULATION DE LA PILE AVEC CLAUSE LEARNING **)


(* Remonte la pile et fait toutes les opérations correspondantes sur clause et pos *)
let rec separate stack clause solution =
	match stack with
	| [] -> ()
	| v::tail ->
		solution.(abs v) <- v ;
		clause := change_clause !clause solution ;
		separate tail clause solution


(* Traite la clause "clause" comme si elle avait été présente depuis le début de l'exécution dans current *)
let maj_clause_learning stack clause levels =
	let stack_rev = List.rev !stack in
	let new_clause = ref clause in
	let solution = Array.make (Array.length levels) 0 in	(* Ersatz de solution, pour les fonctions dans Watched *)
	separate stack_rev new_clause solution ;
	!new_clause



(* Dans le cas où l'on doit remonter jusqu'au début de l'exécution, vide la pile et signifie à la boucle
   principale doit pas s'arrêter.                                                                          *)
let from_scratch stack current solution levels k para =
	(* Si k est nul, alors la contradiction est obtenue sans faire de paris, on ne fait donc rien pour
	   continuer l'algorithme car on sait que la CNF n'est pas satisfiable.                            *)
	if (!k != 0) then
		begin
		while !k != 0 do
			if abs solution.(abs !k) = 1 then
				para.level <- para.level - 1 ;
			solution.(abs !k) <- 0 ;
			k := backtrack stack ;
			k := pick stack
		done ;
		
		(* On place k à 0 pour que l'agorithme ne s'arrête pas encore, et solution.(0) à 1 pour
		   indiquer à la boucle principale de placer k à 0.                                       *)
		k := 1 ;
		solution.(0) <- 1
		end
