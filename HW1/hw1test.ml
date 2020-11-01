(* HW1 TESTS *)

(* Problem 1 *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1;2;3]
let my_subset_test2 = subset [2;3;1] [1;2;3]
let my_subset_test3 = not (subset [4] [1;2;3])

(* Problem 2 *)
let my_equal_sets_test0 = equal_sets [1;2;3] [1;2;3]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [4;5;6])

(* Problem 3 *)
let my_set_union_test0 = equal_sets (set_union [1;4] [1;2;3]) [1;2;3;4]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1] []) [1]

(* Problem 4 *)
let my_set_intersection_test0 =
  equal_sets (set_intersection [1] [1;2;3]) [1]
let my_set_intersection_test1 =
  equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [4;3;2;1]) [1;3;4;2]

(* Problem 5 *)
let my_set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let my_set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let my_set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []

(* Problem 6 *)
let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 10 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity

(* Problem 7 *)
type avengers_nonterminals =
	| Avengers | Avenger | Ironman | Hulk | Captain

let avengers_grammar =
	Avengers,
	[Ironman, [T"I am Ironman"];
	Hulk, [T"Krrrrrr"];
	Captain, [T"I can do this all day"];
	Avenger, [N Ironman];
	Avenger, [N Hulk];
	Avenger, [N Captain];
	Avengers, [N Avenger];
	Avengers, [N Avenger; T","; N Avengers]]

let my_filter_reachable_test0 =
	filter_reachable avengers_grammar = avengers_grammar

let my_filter_reachable_test1 = 
	filter_reachable (Avenger, (snd avengers_grammar)) =
	(Avenger,
	[Ironman, [T"I am Ironman"];
	Hulk, [T"Krrrrrr"];
	Captain, [T"I can do this all day"];
	Avenger, [N Ironman];
	Avenger, [N Hulk];
	Avenger, [N Captain];])