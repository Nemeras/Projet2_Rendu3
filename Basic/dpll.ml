			(** ALGORITHME DPLL **)

(* La structure principale de l'algorithme DPLL ; la pile est gérée dans stack.ml, les étapes intermédiaires le sont dans step.ml *)


open List

open Parameters
open DynArray
open Cnf
open Step
open Stack
open Print_step



		(** STRUCTURE DE DONNEE - VARIABLES **)


type changing_clauses = (bool*clause*(int list)) dynarray


(* On manipule les clauses dans le tableau dynamique current.
   fst current.a.(i) indique si la clause est actuellement satisfaite, et snd current.a.(i)
   est la clause d'indice i courante, qui peut être modifiée pendant l'exécution de l'algorithme.
   La troième composante est la liste des littéraux de la clause qui ont été mis à faux, dans
   l'ordre chronologique décroissant.                                                             *)

(* Variables utilisées dans la suite :
	stack : la pile des affectations.
	k : référence du littéral considéré à chaque étape.
	level : référence du numéro de niveau de décision courant.
	back : référence de booléen valant vrai si l'algorithme est en phase de backtracking,
		vrai sinon.
	nb_back : référence indiquant le nombre obligatoire de niveaux de décision à remonter dans
		le backtrack.
	learning : vrai si le clause learning est activé, faux sinon.
	draw : (référence) vrai si le mode interactif du clause learning est activé, faux sinon.
	unsat : vrai si l'explication de l'insatisfiabilité est activée, faux sinon.
	print : vrai si l'affichage est activé, faux sinon.
	para : ensemble de paramètres (cf parameters.ml).
   Les fonctions update et backtrack sont explicitées dans stack.ml.                              *)

(* solution désigne dans la suite l'instanciation courante des variables :
	solution.(k) = 1 si la variable k est à vrai suite à un pari / une hypothèse.
	             > 1 si la variable k est à vrai suite à une propagation / déduction.
	solution.(k) = -1 si la variable k est à faux suite à un pari / une hypothèse.
	             < -1 si la variable k est à faux suite à une propagation / déduction.
	solution.(k) = 0 si la valeur de la variable k est indéterminée .
	solution.(0) < 0 si une contradiction a été trouvée ; alors, -solution.(0)-1 est l'
		indice de la clause correspondante.
   Si la abs solution.(k) > 1, alors abs solution.(k) - 2 désigne la clause qui a entraîné la
   propagation sur la variable k.                                                                 *)

(* levels assigne à chaque variable le niveau de décision auquel elle a été déterminé.
   orders.(k) = 0 si k est un pari, et n si k est la n-ème variable a avoit été déterminée suite
   à une propagation dans le niveau de décision levels.(k).
   origins est un dynarray de longueur égale à current. origins.a.(i) contient la liste des
   clauses qui ont été utilisées pour créer la clause i à partir d'une chaîne de résolution.      *)




		(** INITIALISATION **)


(* Renvoie le tableau current correspondant à la CNF cnf, ainsi que le tableau clauses
   qui restera la copie des clauses du tableau current initial.
   Renvoie également un tableau pos : pos.(i) = l1, l2, où l1 est la liste des indices
   des clauses contenant le littéral i, et l2 la liste de celles contenant le littéral -i *)
let cnf_to_vect cnf solution =
	let clauses = DynArray.make(List.length cnf.clauses) [] in
	let current = DynArray.make (List.length cnf.clauses) (false,[],[]) in
	let pos = Array.make (cnf.v_real+1) ([],[]) in
	
	(* Enumère chaque clause et met à jour current et pos *)
	let rec aux clauses_list i =
		match clauses_list with
		| [] -> ()
		| []::_ ->
			solution.(0) <- -1-i ;		(* Clause vide rencontrée : cnf n'est pas satisfiable *)
			clauses.a.(i) <- [] ;
			current.a.(i) <- false, [], []
		| c::tail ->
			activate_clause c pos i ;	(* Mise à jour de pos *)
			clauses.a.(i) <- c ;
			current.a.(i) <- false, c, [] ;
			aux tail (i+1)
	in
	
	aux cnf.clauses 0 ;
	clauses, current, pos




		(** RESOLUTION - STRUCTURE DE DPLL **)



(* Renvoie une solution associée à la CNF cnf donnée en entrée :
	False si cnf n'est pas satisfiable.
	True solution si cnf est satisfiable, avec solution une instanciation qui la satisfait. *)

let solve cnf learning draw unsat print =
	
	(* Tri des littéraux dans les clauses par indice de variable croissant,
	   élimination des tautologies.                                         *)
	ordo cnf ;
	
	(* Initialisation de current, pos, solution et de la pile des instanciations *)
	let solution = Array.make (cnf.v_real+1) 0 in
	let levels = Array.make (cnf.v_real+1) 0 in
	let orders = Array.make (cnf.v_real+1) 0 in
	let clauses, current, pos = cnf_to_vect cnf solution in
	let l = current.length in		(* Nombre de clauses dans la CNF initiale *)
	let stack = create_stack () in		(* stack contient initialement 0 en fond de pile *)
	
	let origins =
		if unsat then
			DynArray.make clauses.length []
		else
			DynArray.make 0 []
	in
	
	let k = ref (
		if solution.(0) < 0 then	(* Si la clause vide est dans la CNF *)
			0
		else
			1
	) in
	let compt = ref 0 in
	
	(* Paramètres des algorithmes de Step *)
	let para = {
		back = false ;
		nb_back = 0 ;
		level = -1 ;
		learning = learning ;
		draw = draw ;
		unsat = unsat ;
		print = print
	} in
	
	(* Boucle principale *)
	while abs !k <= cnf.v_real && !k <> 0 do
		incr compt ;
		
		(* Si on est revenu au début des paris car on a appris une clause unitaire, on réinitialise
		   les valeurs et on reprend l'exécution.                                                   *)
		if solution.(0) > 0 then
			begin
			solution.(0) <- 0 ;
			para.back <- false ;
			para.level <- -1 ;
			k := 0
			end
		;
		
		(* Affichage, si autorisé *)
		if para.print then
			print_step current solution para.back !compt ;
		
		(* Détection des clauses unitaires *)
		if solution.(0) = 0 then
			propa stack current pos solution levels orders para ;
		
		(* Si toutes les variables ont été instanciées *)
		if abs !k = cnf.v_real then
			(* S'il y a contradiction : backtrack *)
			if solution.(0) < 0 then
				continue stack clauses current pos solution levels orders k para origins
			(* Sinon : c'est fini *)
			else
				k := cnf.v_real + 1
		(* Sinon : on continue *)
		else
			continue stack clauses current pos solution levels orders k para origins
		
	done ;
	
	if !k = 0 then
		begin
		(* Preuve de l'insatisfiabilité *)
		if para.unsat then
			Unsat.create_unsat origins clauses solution l ;
		False
		end
	else
		True solution
