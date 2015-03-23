			(** FONCTIONS D'AFFICHAGE **)



open List
open Array
open Printf

open Cnf
open Watched
open DynArray


		(** RECAPITULATIF A CHAQUE ITERATION DE LA BOUCLE **)

let rec print_w_clause c solution =
	let rec aux c solution =
		match c with
		| x::c2 ->
			printf "%d " x ;
			aux c2 solution
		| [] -> print_string "\t0\n"
	in
	match c with
	| h::h2::c2 ->
		printf "\t[| %d (%d)\t|\t%d (%d) |]\t"
				h (max (-2) (min 2 (h*solution.(abs h))))
				h2 (max (-2) (min 2 (h2*solution.(abs h2)))) ;
		aux c2 solution
	| _ -> aux c solution

let print_clauses current solution back =
	print_string "Contradiction : " ;
	if solution.(0) >= 0 then
		print_string "Non.\n"
	else
		print_string "Oui.\n"
	;
	print_string "Backtrack : " ;
	if not back then
		print_string "Non.\n\n"
	else
		print_string "Oui.\n\n"
	;
	for i = 0 to current.length - 1 do
		printf "Clause %d : " i ;
		if is_w_true current.a.(i) solution then
			print_string "[INACTIVE]\t" ;
		print_w_clause current.a.(i) solution
	done

let print_step current solution back compt =
	printf "\n\n*************** \tEtape %d\t\t*****************\n\n" compt ;
	print_string "Solution courante :\n\t" ;
	print_instanciation solution ;
	print_string "\n\n\tEnsemble des clauses étudiées :\n\n" ;
	print_clauses current solution back ;
	print_string "\n\n"



		(** PARIS / CONSEQUENCES **)

let print_hyp k print =
	if print then
		printf "\t****  Hypothèse : %d  ****\n" k

let print_conseq k print =
	if print then
		printf "\t****  Déduction : %d  ****\n" k

let print_new_backtrack print =
	if print then
		begin
		printf "\t****  Contradiction-  ****\n" ;
		printf "\t**** Début  Backtrack ****\n"
		end

let print_backtrack k value print =
	if print then
		begin
		printf "\t****  Backtrack : %d  ****\n" k ;
		if abs value >= 2 then
			print_string "\t[Ancienne déduction]\n" ;
		if abs value >= 2 || k < 0 then
			print_string "\t Le backtrack continue...\n"
		end

let print_learning new_clause print =
	if print then
		printf "\n Clause apprise :\n%s" (Cnf.string_of_clause new_clause)
