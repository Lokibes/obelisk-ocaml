(* curry funcs *)
let avg a b = (a +. b) /. 2.0
avg 3.0 4.6
let avg_3 = avg 3.0
avg_3 4.0

(* match exprs *)
let detect a = 
	match a with
		| [] -> "wrong password :D"
		| head::tail -> if a = [1;2;3;4] then "correct ! welcome" else "wrong password"
detect [1;2;3;4]

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

