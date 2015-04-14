		(** INITIALISATION **)

open Parameters
open Cnf

module Init_clauses (E : Clauses) =
struct

	(* Renvoie le tableau current correspondant à la liste de clauses "clauses". *)
	let cnf_to_vect cnf solution =
		let clauses = DynArray.make (List.length cnf.clauses) [] in
		let current = DynArray.make (List.length cnf.clauses) (E.init_value []) in
		let pos = Array.make (cnf.v_real+1) ([],[]) in
		let rec aux cl i =
			match cl with
			| [] -> ()
			| []::_ ->
				solution.(0) <- -1-i	(* Clause vide rencontrée : cnf n'est pas satisfiable *)
			| c::tail ->
				current.a.(i) <- (E.init_value c) ;
				clauses.a.(i) <- c ;
				E.activate c pos i ;	(* Mise à jour de pos *)
				aux tail (i+1)
		in
		aux cnf.clauses 0 ;
		clauses, current, pos

end



