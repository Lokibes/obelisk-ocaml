(* curry funcs *)
let avg a b = (a +. b) /. 2.0
avg 3.0 4.6
let avg_3 = avg 3.0
avg_3 4.0

let a = string_of_int 88

(* match exprs *)
let detect a = 
	match a with
		| [] -> "wrong password :D"
		| head::tail -> if a = [1;2;3;4] then "correct ! welcome" else "wrong password"
detect [1;2;3;4]
let guess a b = 
	match a with
		| "a" -> if b = 0 then "U are" ^ "... right ^^" else ""
		| _ -> "Absolutely wrong :D"
guess "a" 0

(* recursive *)
(* find Fibonacci Nth *)let rec fibo n =
	match n with
		| 0 -> 1
		| 1 -> 1
		| n -> fibo(n-1) + fibo(n-2)
fibo 5

(* sort a List insertion *)let rec sort list =
	match list with
		| [] -> []
		| head::tail -> insert head (sort tail)
	and insert a b =
		match b with
			| [] -> a::[]
			| headB::tailB -> if headB < a then headB::(insert a tailB) else a::b
sort [3;5;8;2;3;6;1;5;3]

(* from lab ex *)
let num_list = [1;3;2;6;4;7;8;9]
List.length num_list
List.nth num_list 3
List.map
List.map (fun x -> x * 2) num_list

(* concatenation *)
let l' = [1;2;3] @ [4;5;6]

(* push an element at the beginning *)
let l' = 1 :: [2;3;4;5;6];

(* try redefine the length of a list : *)
let rec my_length l =
  match l with
  | [] -> 0
  | x::l' -> 1 + (my_length l')
my_length [1;2;3;4]

(* push an element at the ENDing *)
let add_last a list = 
	match list with
		| [] -> a::[]
		| h::t -> list @ a::[]
add_last 6 [2;3;4;5]
sort (add_last 5 [3;4;5;2;1;7;9;])

(* play around with TYPE in OCaml *)
type pair_int = { a : int; b : int }
{a = 4; b = 8}
{a = 4; b=9}

let detect2 pair = 
	match pair with
		| {a=0;b=0} -> "just kidding ^^"
		| {a=7;b=7} -> "oh ! U won !!"
		| _ -> "hey, be serious !"
detect2 {a=1;b=8}

type pair_int = Nothing | Int of int | P_Int of int * int
Nothing
Int 3
P_Int (5, 8)

let detect2 pair = 
	match pair with
		| Nothing -> "Hey U, be serious !"
		| Int a -> if a = 0 then "good luck next time !" else "!"
		| P_Int (a, b) -> if (a > 0) then 
			begin
				if (b > 0) then "congratulation !" else "so pitiful !"
			end
			else "Con ... ! Oh, never mind !"

detect2 (P_Int (5, 0))

(* applies TYPE to a binary Tree *)
type bin_tree = Leaf of int | Tree of bin_tree * bin_tree
let rec find a aTree = 
	match aTree with
		| Leaf x -> if x = a then true else false
		| Tree (x, y) -> if (find a x) then true else (find a y)
let rec cut_leaf leaf tree = 
	match tree with
		| Leaf x -> if x = leaf then (Leaf 0) else (Leaf x)
		| Tree (x, y) -> Tree (cut_leaf leaf x, cut_leaf leaf y) 
let t1 = Leaf 6
let t2 = Tree (Leaf 6, Leaf 5)
let t3 = Tree (Tree (Leaf 7, Leaf 2), Tree (Leaf 4, Leaf 6))
find 4 t2
cut_leaf 6 t3
cut_leaf 4 (cut_leaf 0 t3)
cut_leaf "I" love (* wrong *)

(* or , abstractedly : *)
type 'a b_tree = Leaf of 'a | Tree of 'a b_tree * 'a b_tree
let love = Tree (Leaf "I", Tree (Leaf "love", Leaf "U"))
type 'a list = Nil | :: of 'a * 'a list
Nil
1::5::6::Nil

(* build a simple code-detecter *)
type exp = 
	| Plu of exp * exp
	| Mul of exp * exp
	| Min of exp * exp
	| Div of exp * exp
	| V of string
let rec print_down s =
	match s with
		| Plu (a, b) -> "(" ^ (print_down a) ^ "+" ^ (print_down b) ^ ")"
		| Min (a, b) -> "(" ^ (print_down a) ^ "-" ^ (print_down b) ^ ")"
		| Mul (a, b) -> "(" ^ (print_down a) ^ "*" ^ (print_down b) ^ ")"
		| Div (a, b) -> "(" ^ (print_down a) ^ "/" ^ (print_down b) ^ ")"
		| V str -> str
let reform exp = print_endline("You wrote : " ^ (print_down exp))
reform (Plu ((Mul (V "a", V "b")), Div (V "c", V "d")))
reform (Plu (Mul (V "e1", V "e2"), Mul (V "e3", V "e4")))