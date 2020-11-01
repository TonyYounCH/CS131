type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let accept_all string = Some string;;
let accept_empty_suffix x  = match x with
	| _::_ -> None
	| x -> Some x;;



(* Assignment 1 *)
(* production function that generates alternative list *)
let rec production_function rule nonterm =  match rule with
	| [] -> []
	| h::t -> if (fst h) = nonterm then (snd h)::(production_function t nonterm)
			  else (production_function t nonterm);;

let convert_grammar gram1 = 
	(fst gram1), (production_function (snd gram1));;

(* Assignment 2 *)

let rec parse_tree_list tree_list = match tree_list with
	| [] -> []
	| h::t -> match h with
		| Node (nonterm, sub_list) -> parse_tree_list sub_list@(parse_tree_list t)
		| Leaf l -> l::(parse_tree_list t);;

let parse_tree_leaves tree = parse_tree_list [tree];;

(* Assignment 3 *)

let rec matcher production rules accept frag = match rules with
	| [] -> None
	| h_rules::t_rules -> 
		let return = match_rule production h_rules accept frag in
		if return = None then matcher production t_rules accept frag
		else return
and match_rule production rule accept frag = match rule with
	| [] -> accept frag
	| h_rule::t_rule -> match h_rule with
		| N nonterm -> matcher production (production nonterm) (match_rule production t_rule accept) frag
		| T term -> match frag with
			| [] -> None
			| h_frag::t_frag -> if term = h_frag then match_rule production t_rule accept t_frag 
								else None;;

let make_matcher gram = 
	matcher (snd gram) ((snd gram) (fst gram))


(* Assignment 4 *)

let acceptor tree frag = match frag with
	| [] -> Some tree
	| _ -> None;;

let rec parser current production rules accept tree frag = match rules with
	| [] -> None
	| h_rules::t_rules -> 
		let return = parse_rule current production h_rules accept ((current, h_rules)::tree) frag in
		if return = None then parser current production t_rules accept tree frag
		else return 
and parse_rule current production rule accept tree frag = match rule with
	| [] -> accept tree frag
	| h_rule::t_rule -> match h_rule with
		| N nonterm -> 
			parser nonterm production (production nonterm) (parse_rule current production t_rule accept) tree frag
		| T term -> match frag with
			| [] -> None
			| h_frag::t_frag -> 
				if term = h_frag then parse_rule current production t_rule accept tree t_frag 
				else None;;

(* Some
	[
		(Num, [T "2"]); 
		(Term, [N Num]); 
		(Expr, [N Term]); 
		(Binop, [T "-"]);
		(Incrop, [T "++"]); 
		(Num, [T "1"]); 
		(Term, [N Num]); 
		(Expr, [N Term]);
		(Lvalue, [T "$"; N Expr]); 
		(Term, [N Lvalue; N Incrop]);
		(Expr, [N Term; N Binop; N Expr])
	]
 *)
(* 
Node (Expr,
		[
		 	Node (Term, [
		 				Node (Lvalue, [ Leaf "$"; Node (Expr, [ Node (Term, [ Node (Num, [Leaf "1"])])])]);
			 			Node (Incrop, [Leaf "++"])
			 			]);
		  	Node (Binop, [Leaf "-"] );
		 	Node (Expr, [ Node (Term, [Node (Num, [Leaf "2"])])])
		]
	)
 *)

let rec traverse_tree tree = match tree with
	| [] -> Leaf "This should never be reached", []
	| h_tree::t_tree -> 
		let t, rest = traverse_rule (snd h_tree) t_tree in
		Node ((fst h_tree), t), rest
and traverse_rule rule rest_tree = match rule with
	| [] -> [], rest_tree
	| h_rule::t_rule -> match h_rule with
		| N nonterm -> 
			let result_node, new_rest = traverse_tree rest_tree in
			let result, new_rest2 = traverse_rule t_rule new_rest in
			result_node::result, new_rest2
		| T term -> 
			let result, new_rest = traverse_rule t_rule rest_tree in
			(Leaf term)::result, new_rest;;


let make_parser_helper parsed_rules frag = 
	let tree = parsed_rules frag in
	match tree with
		| Some t -> 
			let rev_tree = List.rev t in
			let result, _ = traverse_tree rev_tree in
			Some result
		| _ -> None;;

let make_parser gram = 
	let start = (fst gram) in
	let production = (snd gram) in
	let rules = production start in
	make_parser_helper (parser start production rules acceptor []);;

