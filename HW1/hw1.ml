(* Problem 1 *)
let rec subset a b : bool = match a with
	| [] -> true
	| h::t -> if List.mem h b then subset t b 
			  else false;;

(* Problem 2 *)
let rec equal_sets a b : bool = 
	subset a b && subset b a

(* Problem 3 *)
let rec set_union a b = match a with
	| [] -> b
	| h::t -> if List.mem h b then set_union t b 
			  else h::set_union t b;;

(* Problem 4 *)
let rec set_intersection a b = match a with
	| [] -> []
	| h::t -> if List.mem h b then h::set_intersection t b
			  else set_intersection t b;;


(* Problem 5 *)
let rec set_diff a b = match a with
	| [] -> []
	| h::t -> if List.mem h b then set_diff t b 
			  else h::set_diff t b;;

(* Problem 6 *)
let rec computed_fixed_point eq f x = 
	if eq (f x) x then x
	else computed_fixed_point eq f (f x);;


(* Problem 7 *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let rec cleaned lst_of_symbol = match lst_of_symbol with
	| [] -> []
	| h::t -> 
		let cleaned_res_symbols = cleaned t in
		match h with
			| N sym -> sym::cleaned_res_symbols
			| T _ -> cleaned_res_symbols;;

let rec nts l = match l with
	| [] -> []
	| h::t -> (cleaned (snd h))@nts t;;

let rec get_all_nonterminals prev next l = match next with
	| [] -> []
	| h::t -> 
		let k = List.filter(fun x -> (fst x) = h) l in
		let p = h::prev in
		let n = set_diff (set_union (nts k) next) p in
		set_union p (get_all_nonterminals p n l);;


let rec list_compare gram nonterminals = match gram with
	| [] -> []
	| h::t -> if List.mem (fst h) nonterminals 
			then h::list_compare t nonterminals
			else list_compare t nonterminals;;

let filter_reachable g = 
	let start = (fst g) in
	let nonterminals = get_all_nonterminals [] (start::[]) (snd g) in
	start, list_compare (snd g) nonterminals;;

