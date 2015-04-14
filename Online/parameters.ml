open DynArray

type pos = ((int list) * (int list)) array


module type Clauses =
sig
	type t
	type stack
	val init_value : Cnf.clause -> t
	val init_stack : unit -> stack
	val pick : stack -> Cnf.literal
	val activate : Cnf.clause -> pos -> int -> unit
	val units : t dynarray -> int array -> (int*int) list ref -> unit
	val update : Cnf.literal -> stack -> t dynarray -> pos -> int array -> (int * int) list
	val backtrack : stack -> t dynarray -> pos -> int array -> Cnf.literal
	val edges : int -> Dot.graph -> t dynarray -> int array -> int array -> int array -> bool array -> Cnf.literal -> int -> int -> unit
	val maj_cl : stack -> Cnf.clause -> pos -> int array -> int -> t
	val print_clause : t -> int array -> unit
	val is_clause_true : t -> int array -> bool
end



let version wl =
	if not wl then
		(module Clauses_basic : Clauses)
	else
		(module Clauses_wl : Clauses)





		(** PARAMETRES DES PROGRAMMES **)

(* Evite de donner plus de 10 arguments à une seule fonction *)

type parameters = {
	mutable back : bool ;
	mutable nb_back : int ;
	mutable level : int ;
	wl : bool ;
	learning : bool ;
	mutable draw : bool ;
	unsat : bool ;
	print : bool
}




module type Theory =
sig
	type atom
	type struc
	val create : string -> Cnf.cnf * struc
	val update : struc -> Cnf.literal -> int
	val backtrack : struc -> Cnf.literal -> unit
	val unsat : struc -> Cnf.clause
	val print_solution : Cnf.solution -> struc -> unit
end


let choose_theory theory =
	match theory with
	| 0 -> (module Base : Theory)
	| 1 -> (module Empty : Theory)
	(*| 2 -> (module Equality : Theory)*)
	| _ -> failwith "Erreur dans le choix de la théorie"
