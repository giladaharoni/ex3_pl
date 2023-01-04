(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91));;


let fresh_var used_vars : string = 
	if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
	then raise (OutOfVariablesError)
	else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars);;



(*
  ADD FUNCTIONS BELOW
*)

let rec fv term1 = match term1 with
								| Variable s -> StringSet.add s StringSet.empty
								| Abstraction (s, t) -> StringSet.remove s (fv t)
								| Application (t1, t2) -> StringSet.union (fv t1) (fv t2);;


let rec substitute x t1 t2 = match t2 with
							| Variable y -> if x = y then t1 else t2
							| Abstraction (y, t) ->
									if x = y then
										t2
									else if StringSet.mem y (fv t1) then
										let z = fresh_var (StringSet.union (fv t1) (fv t)) in
										Abstraction (z, substitute x t1 (substitute y (Variable z) t))
									else
										Abstraction (y, substitute x t1 t)
							| Application (t3, t4) ->
									Application (substitute x t1 t3, substitute x t1 t4);;



let rec reduce_cbv term = match term with
			| Variable _ -> None (* no reduction possible for variables *)
			| Abstraction (s, t) -> None
			| Application (Abstraction (s, t), t2) -> (match reduce_cbv t2 with
				| Some t2' -> Some (Application (Abstraction (s, t), t2'))
				| None -> Some (substitute s t2 t))
			| Application (t1, t2) -> (match reduce_cbv t1 with
				| Some t1' -> Some (Application (t1', t2))
				| None -> (match reduce_cbv t2 with
					| Some t2' -> Some (Application (t1, t2'))
					| None -> None));;



let rec reduce_cbn term = match term with
	| Variable _ -> None (* no reduction possible for variables *)
	| Abstraction (s, t) -> None
	| Application (Abstraction (s, t), t2) -> Some (substitute s t2 t)
	| Application (t1, t2) -> (match reduce_cbn t1 with
		| Some t1' -> Some (Application (t1', t2))
		| None -> (match reduce_cbn t2 with
			| Some t2' -> Some (Application (t1, t2'))
			| None -> None));;
