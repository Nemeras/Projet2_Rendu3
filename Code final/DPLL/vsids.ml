		(** HEURISTIQUE VSIDS **)


(* On utilise pour implémenter VSIDS un tableau donnant pour chaque littéral son score,
   et un Set de couples (score, littéral) permettant de les trier et de choisir celui de plus grand score *)

open Types
open Types.S


(* Donne le score de x *)
let score x para =
	if x > 0 then
		para.scores.(x)
	else
		para.scores.(Array.length para.scores + x)


(* Indique si x est dans le Set para.vsids *)
let is_in x para =
	mem (score x para, x) para.set_vsids


(* Ajoute x dans le Set *)
let add_score x para =
	para.set_vsids <- add (score x para, x) para.set_vsids


(* Enlève x du Set *)
let remove_score x para =
	para.set_vsids <- remove (score x para, x) para.set_vsids


(* Initie para.scores et para.vsids *)
let create pos para =
	let n = Array.length pos - 1 in
	para.scores <- Array.make (2*n+1) 0 ;
	para.set_vsids <- empty ;
	for i = 1 to n do
		(* Le score du littréal x sera le nombre clauses dans lequel il apparaît (ou est surveillé pour les WL *)
		para.scores.(i) <- List.length (fst pos.(i)) ;
		para.scores.(2*n-i+1) <- List.length (snd pos.(i)) ;
		add_score i para ;
		add_score (-i) para
	done


(* Attribue à x le score s *)
let modif x s para =
	let b = is_in x para in
	if b then
		remove_score x para ;
	if x > 0 then
		para.scores.(x) <- s
	else
		para.scores.(Array.length para.scores + x) <- s ;
	if b then
		add_score x para


(* Augmente d'un le score du littréal x *)
let incr_li x para =
	modif x (score x para + 1) para


(* Augmente d'un le score des littéraux de la clause c *)
let rec incr c para =
	match c with
	| [] -> ()
	| x::c2 ->
		incr_li x para ;
		incr c2 para


(* Multiplie tous les scores par 0.9 (à chaque conflit) *)
let decr_scores para =
	for i = 1 to (Array.length para.scores)/2 do
		modif i (int_of_float ((float_of_int (score i para))*.0.9)) para ;
		modif (-i) (int_of_float ((float_of_int (score (-i) para))*.0.9)) para
	done


(* Donne le littéral de plus gros score (min_elt car on a pris l'opposé de compare dans Types.Lit) *)
let next para =
	snd (min_elt para.set_vsids)
