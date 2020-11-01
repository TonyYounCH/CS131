
type clothes_nonterminals =
	| Clothes | Outer | Shirts | Pants | Jacket | Hoodie | Tshirts | Shorts | Jeans


let clothes_grammar = 
	(Clothes,
		function
			| Clothes ->
				[
					[N Pants; N Outer];
					[N Outer; N Shirts; N Pants];
				 	[N Shirts; N Pants]
				]
			| Outer ->
				[
					[N Jacket];
					[N Hoodie];
					[N Jacket; N Hoodie]
				]
			| Shirts ->
				[
					[N Tshirts];
				]
			| Pants ->
				[
					[N Shorts];
					[N Jeans]
				]
			| Jacket ->
				[
					[T"Black Jacket"];
					[T"Blue Jacket"];
				]
			| Hoodie ->
				[
					[T"UCLA Hoodie"];
					[T"USC Hoodie"];
				]
			| Tshirts ->
				[
					[T"Long-Sleeve"];
					[T"Short-Sleeve"];
				]
			| Shorts ->
				[
					[T"Hawaiian"];
					[T"Swim Trunks"]
				]
			| Jeans ->
				[
					[T"Levi's Jeans"];
					[T"Gucci Jeans"]
				]
	)



(* Assignment 5 *)
let make_matcher_test = 
((make_matcher clothes_grammar accept_all ["Swim Trunks"; "Black Jacket"; "UCLA Hoodie"]) = Some ["UCLA Hoodie"])


(* Assignment 6 *)
let make_parser_test = 
match make_parser clothes_grammar ["Swim Trunks"; "Black Jacket"; "UCLA Hoodie"] with
	| Some tree -> ["Swim Trunks"; "Black Jacket"; "UCLA Hoodie"] = parse_tree_leaves tree
	| _ -> false;;










