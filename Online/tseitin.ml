open Printf

type formula =
	| Lit of int*int
	| And of formula*formula*int
	| Or of formula*formula*int
	| Not of formula*int

type form = formula*int

type formlist = (formula list) * int


let numeroter formula debut =	
	let numerotage = ref debut in
	let rec num form nvar =
		match form with
		| Lit (a,b) ->
			incr nvar ;
			let temp = !nvar in
			Lit (a, temp)
		| And (a,b,c) ->
			incr nvar ;
			let temp = !nvar in
			And (num a nvar, num b nvar, temp)
		| Or (a,b,c) ->
			incr nvar ;
			let temp = !nvar in
			Or (num a nvar, num b nvar, temp)
		| Not (a,b) ->
			incr nvar ;
			let temp = !nvar in
			Not(num a nvar, temp)
	in
 	let rec num_aux liste nvar =
		match liste with
		| tree::other ->
			(num tree nvar)::(num_aux other nvar)
		| [] -> []
	in
	num_aux formula numerotage

let etage form =
	match form with
	| Lit (a,b) -> b
	| And (a,b,c) -> c
	| Or (a,b,c) -> c
	| Not (a,b) -> b

let rec print_form tree buffer =
	match tree with
	| Lit (a,b) ->
		bprintf buffer "-%d %d 0\n%d -%d 0\n" a b a b
	| And (a,b,c) ->
		bprintf buffer "-%d %d 0\n-%d %d 0\n%d %d %d 0\n" c (etage a) c (etage b) c (etage a) (etage b) ;
		print_form a buffer ;
		print_form b buffer
	| Or (a,b,c) ->
		bprintf buffer "-%d %d %d 0\n%d -%d 0\n%d -%d 0\n" c (etage a) (etage b) c (etage a) c (etage b) ;
		print_form a buffer ;
		print_form b buffer
	| Not (Lit (a,b), c) ->
		bprintf buffer "-%d -%d 0\n%d %d 0\n" c a c a
	| Not (a,b) ->
		bprintf buffer "-%d -%d 0\n%d %d 0\n" (etage a) b (etage a) b ;
		print_form a buffer


let rec conv_liste formula_list buffer=
	match formula_list with
	| tree::other ->
		bprintf buffer "%d 0\n" (etage tree) ;
		print_form tree buffer ;
		conv_liste other buffer
	| [] -> ()


let conv_tseitin formula_list m =
	let buffer = Buffer.create 1 in
	bprintf buffer "p cnf 0 0\n" ;
	conv_liste (numeroter formula_list m) buffer ;
	Buffer.contents buffer

