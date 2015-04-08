type formula =
	| Lit of int*int
	| And of formula*formula*int
	| Or of formula*formula*int
	| Not of formula*int;;

type form = formula*int;;

type formlist = (formula list) * int
