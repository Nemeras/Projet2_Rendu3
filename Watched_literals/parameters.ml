			(** PARAMETRES DES PROGRAMMES **)

(* Evite de donner plus de 10 arguments à une seule fonction *)

type parameters = {
	mutable back : bool ;
	mutable nb_back : int ;
	mutable level : int ;
	learning : bool ;
	mutable draw : bool ;
	unsat : bool ;
	print : bool
}
