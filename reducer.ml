(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
	if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
	then raise (OutOfVariablesError)
	else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)
(*
  ADD FUNCTIONS BELOW
*)



let rec fv term1 = match term1 with
								| Variable s -> add s StringSet.empty
								| Abstraction (s, t) -> StringSet.remove s (fv t)
								| Application (t1, t2) -> StringSet.union (fv t1) (fv t2)

let rec substitute x t1 t2 =
  							let fresh_x = if StringSet.mem x (fv t1) then fresh_var (fv t2) else x in
  							let t1' = if fresh_x = x then t1 else substitute x (Variable fresh_x) t1 in
								match t2 with 
							| Variable y -> if x=y then t1' else t2
							| Abstraction (y, t) -> if x = y then t2 else Abstraction (y, substitute x t1' t)
							| Application (t2', t2'') ->  Application (substitute x t1' t2', substitute x t1' t2'')

