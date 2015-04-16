open Tseitin


type atom =
	| Eg of int*int
	| Ineg of int*int
  
type change =
	| Addineg of int*int*int
	| Addeg of int*int*int*int*int

type uf= (int*int) array
		  
type inegs = (int*int) list

type stack = change list
		      
let rec racine n arr =
	match fst (arr.(n)) with
	| x when x = n -> x
	| x -> racine x arr
  
  
type struc = {
	mutable aa : atom array ;
	mutable eg : uf ;
	mutable ineg : inegs ;
	mutable st : stack ;
	mutable unsat : Cnf.clause
}
	   

let max_var arr =
	let maxv = ref 0 in 
	for i=0 to Array.length arr - 1 do
		match (arr.(i)) with
		|Eg(x,y) -> maxv := max !maxv (max x y) ;
		|Ineg(x,y) -> maxv := max !maxv (max x y) ;
	done ;
	!maxv





(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)

let lexstr s =
	Lexing.from_string s

let parse file =
	Parser_equ.formula Lexer_equ.token (lexbuf file)

let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)



module Atom =
	struct
		type t = int * int
		let compare = compare
	end

module KeyAtom = Map.Make (Atom)

open KeyAtom


let analyze f =
	let m = ref empty in
	let rec aux f =
		match f with
		| Lit ((x,y),b) when mem (min x y, max x y) !m ->
			Lit (find (min x y, max x y) !m, b)
		| Lit ((x,y),b) ->
			m := add (min x y, max x y) (cardinal !m + 1) !m ;
			Lit (cardinal !m, b)
		| And (a,b,c) ->
			And (aux a, aux b, c)
		| Or (a,b,c) ->
			Or (aux a, aux b, c)
		| Not (a,b) ->
			Not (aux a, b)
	in
	let f0 = List.map aux f in
	let arr = Array.make (cardinal !m + 1) (Eg(0,0)) in
	while !m <> empty do
		let (x,y), i = choose !m in
		m := remove (x,y) !m ;
		if x >= 0 then
			arr.(i) <- Eg(x,y)
		else
			arr.(i) <- Ineg(-x-1,-y-1)
	done ;
	f0, arr


let create file aff_cnf =
	try
		let f = parse file in
		let f0, arr = analyze f in
		let solver = { aa = arr ; eg = Array.init (1 + max_var arr) (fun i -> (i,1)) ; ineg = [] ; st = [] ; unsat = [] } in
		let s = Tseitin.conv_tseitin f0 (Array.length arr) in
		if aff_cnf then
			begin
			print_string "CNF produite :\n" ;
			print_string s ;
			print_newline ()
			end ;
		let cnf = parse_cnf s in
		cnf, solver
	with _ -> (failwith "Erreur de saisie")














let union x y arr lit =
	match arr.(racine x arr), arr.(racine y arr) with
	| (a,_), (b,_) when a = b -> Addeg (lit,a,b,a,b)
	| (a,sza), (b,szb) when sza > szb ->
		arr.(b) <- (a, szb) ;
		arr.(a) <- (a, sza+szb) ;
		Addeg (lit,x,y,b,a)
	| (a,sza), (b,szb) ->
		arr.(a) <- (b, sza) ;
		arr.(b) <- (b, sza+szb) ;
		Addeg (lit,x,y,a,b)


let modif struc lit =
	match struc.aa.(abs lit) with
	| Eg (x,y) when lit > 0 -> union x y (struc.eg) lit; 
	| Eg (x,y) -> struc.ineg <- (x,y)::(struc.ineg);Addineg(lit,x,y);
	| Ineg (x,y) when lit > 0 -> struc.ineg <- (x,y)::(struc.ineg);(Addineg(lit,x,y));
	| Ineg (x,y) -> union x y (struc.eg) lit
  
let rec check_aux liste arr =
	match (liste) with
	| (x,y)::_ when racine x arr = racine y arr -> (x,y)
	| (x,y)::t -> check_aux t arr
	| [] -> (0,0)

let check struc =
	check_aux (struc.ineg) (struc.eg)

let rec search_i st x y=
	match st with
	| Addeg(_)::t -> search_i t x y
	| Addineg(lit,a,b)::t when a = x && b = y -> lit
	| Addineg(_)::t -> search_i t x y
	| [] -> failwith "problem in search_i"

let rec remp_t sta tableau=
	match sta with
	| (Addineg(_))::t -> remp_t t tableau
	| (Addeg(lit,x,y,a,b))::t -> tableau.(x) <-
		(y,lit)::(tableau.(x)) ;
		tableau.(y) <- (x,lit)::(tableau.(y)) ;
		remp_t t tableau
	| [] -> ()



let chemin tableau debut fin=
	let liste_visit = ref [debut] in
	let actuel = ref (debut,0) in
	let precedent = ref [0,0] in
	while fst !actuel != fin do 
		if tableau.(fst !actuel) = [] then
			begin
			if (!precedent=[0,0]) then
				failwith "pas de chemin" ;
			actuel := (List.hd !precedent) ;
			precedent := List.tl (!precedent) ;
			tableau.(fst !actuel) <- List.tl (tableau.(fst !actuel))
			end
		else
			begin
			if List.exists (fun i -> i = (fst (List.hd tableau.(fst !actuel)))) !liste_visit then
	  			begin
	   			tableau.(fst !actuel) <- List.tl tableau.(fst !actuel)
	 			end
			else
	 			begin
				precedent := (!actuel)::(!precedent) ;
				actuel := List.hd tableau.(fst !actuel) ;
				liste_visit := (fst !actuel)::(!liste_visit)
				end
			end
	done ;
	List.tl (List.tl (List.rev (!actuel::(!precedent))))
    
let backtrack struc lit =
	if abs lit < Array.length struc.aa then
		begin
		let dernier_changement = List.hd struc.st in
		struc.st <- List.tl (struc.st) ;
		match dernier_changement with
		| Addeg (lit,x,y,rx,ry) when rx = ry -> ()
		| Addeg (lit,x,y,rx,ry) ->
			let a,sz = struc.eg.(rx) in
			struc.eg.(rx) <- (rx,sz) ;
			let b,szb = struc.eg.(ry) in 
			struc.eg.(ry) <- (ry,szb-sz)
		| Addineg (_) ->
			struc.ineg <- List.tl (struc.ineg)
		end


let update struc lit =
	if abs lit < Array.length struc.aa then
		begin
		struc.st <- (modif struc lit)::struc.st ;
		let che = check struc in
		if che = (0,0) then
			0
		else
			begin
			let (a,b) = che in
			let lit = search_i (struc.st) a b in
			let tableau_adj = Array.make (Array.length struc.eg) []in
			remp_t (struc.st) tableau_adj;
			let liste_sommet = chemin tableau_adj a b in
			let func x =
				match x with
				| (a,b)-> -b
			in
			let clause = List.map func liste_sommet in
			struc.unsat <- -lit::clause ;
			-max_int
			end
		end
	else
		0


let unsat struc =
	struc.unsat


let color instanciation solver i =
	let rec color_aux i =
		match solver.eg.(i) with
		| j, _ when j = i ->
			instanciation.(i)
		| j, _ ->
			let c = color_aux j in
			instanciation.(i) <- c ;
			solver.eg.(i) <- i, 0 ;
			c
	in
	let _ = color_aux i in
	()

let find_instanciation solver =
	let l = Array.length solver.eg in
	let instanciation = Array.init l (fun i -> i) in
	for i = 0 to l - 1 do
		color instanciation solver i
	done ;
	for i = 0 to l - 1 do
		Printf.printf "x%d = %d\n" i instanciation.(i)
	done
	


let print_solution res solver =
	match res with
	| Cnf.False ->
		print_string "UNSATISFIABLE\n"
	| Cnf.True _ ->
		print_string "SATISFIABLE\n" ;
		find_instanciation solver
