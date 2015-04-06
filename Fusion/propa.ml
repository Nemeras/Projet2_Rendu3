			(** BOOLEAN CONSTRAINT PROPAGATION **)

open Parameters

module Propagation (E : Clauses) =
struct

	(* Trouve toutes les conséquences des clauses unitaires (units) apparues à cette étape *)
	let rec propa stack current pos solution levels orders para =
		
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
				levels.(abs x) <- para.level ;
				orders.(abs x) <- num ;
				let module P = Print_step.Print(E) in
				P.print_conseq x para.print ;
				let l = E.update x stack current pos solution in
				aux (l@tail) (num+1)
			| (x,i)::tail ->
				aux tail num
		in
		
		let uni = ref [] in
		E.units current solution uni ;
		(*print_int (fst (List.hd ( !uni))) ; print_newline() ;*)
	
		(* Détermine la numérotation de départ à placer dans orders *)
		let compt = ref 1 in
		for i = 1 to Array.length solution - 1 do
			if solution.(i) != 0 && levels.(i) = para.level && orders.(i)+1 > !compt then
				compt := orders.(i) + 1 ;
		done ;
	
		aux !uni !compt

end
