open Stdlib

let _  = Random.self_init ()

type term =
  | Constant of string
  | Variable of string
  | Function of string * term list

type head = term
type body = term list

type clause = Fact of head | Rule of head * body

type program = clause list

type goal = term list

let rec string_of_f_list f tl =
  let _, s = List.fold_left (fun (first, s) t ->
    let prefix = if first then "" else s ^ ", " in
    false, prefix ^ (f t)) (true,"") tl
  in
  s

let rec string_of_term t =
  match t with
  | Constant c -> c
  | Variable v -> v
  | Function (f,tl) ->
      f ^ "(" ^ (string_of_f_list string_of_term tl) ^ ")"

let string_of_term_list fl =
  string_of_f_list string_of_term fl

let string_of_goal g =
  "?- " ^ (string_of_term_list g)

let string_of_clause c =
  match c with
  | Fact f -> string_of_term f ^ "."
  | Rule (h,b) -> string_of_term h ^ " :- " ^ (string_of_term_list b) ^ "."

let string_of_program p =
  let rec loop p acc =
    match p with
    | [] -> acc
    | [c] -> acc ^ (string_of_clause c)
    | c::t ->  loop t (acc ^ (string_of_clause c) ^ "\n")
  in loop p ""

let var v = Variable v
let const c = Constant c
let func f l = Function (f,l)
let fact f = Fact f
let rule h b = Rule (h,b)

(* Problem 1 *)

let rec occurs_check v t =
  match t with
  | Constant c -> false
  | Variable v1 -> if v = t then true else false
  | Function (f,tl) -> List.fold_left (fun acc x -> occurs_check v x) false tl ;;

(* Problem 2 *)

module VarSet = Set.Make(struct type t = term let compare = Stdlib.compare end)
(* API Docs for Set : https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.S.html *)

let rec variables_of_term t =
  match t with
  | Constant _ -> VarSet.empty
  | Variable _ -> VarSet.singleton t
  | Function (f,tl) -> List.fold_left (fun acc x -> VarSet.union (variables_of_term x) acc) VarSet.empty tl ;;


let rec varOfC t = 
  match t with
  | Constant _ -> VarSet.empty
  | Variable _ -> VarSet.singleton t
  | Function (f,tl) -> List.fold_left (fun acc x -> VarSet.union (varOfC x) acc) VarSet.empty tl ;;

let variables_of_clause c =
  match c with
  | Fact f -> varOfC f
  | Rule (h,b) -> VarSet.union (varOfC h) (List.fold_left (fun acc x -> VarSet.union (varOfC x) acc) VarSet.empty b) ;;

(* Problem 3 *)

module Substitution = Map.Make(struct type t = term let compare = Stdlib.compare end)
(* See API docs for OCaml Map: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html *)

let string_of_substitution s =
  "{" ^ (
    Substitution.fold (
      fun v t s ->
        match v with
        | Variable v -> s ^ "; " ^ v ^ " -> " ^ (string_of_term t)
        | Constant _ -> assert false (* substitution maps a variable to a term *)
        | Function _ -> assert false (* substitution maps a variable to a term *)
    ) s ""
  ) ^ "}"


let help s t =
  match Substitution.find_opt t s with
        | None -> t
        | Some t' ->  t'

let rec substitute_in_term s t =
  match t with
    | Variable _ -> help s t
    | Constant _ -> t
    | Function (f,tl) -> Function (f, List.map (fun x -> substitute_in_term s x) tl) ;;


let rec sub_in_c s c = 
  match c with
    | Variable _ -> help s c
    | Constant _ -> c
    | Function (f,tl) -> Function (f, List.map (fun x -> sub_in_c s x) tl) ;;

let substitute_in_clause s c =
  match c with
  | Fact f -> (Fact (sub_in_c s f))
  | Rule (h,b) -> (Rule ((sub_in_c s h) , List.map (fun x -> sub_in_c s x) b)) ;;

(* Problem 4 *)

let counter = ref 0
let fresh () =
  let c = !counter in
  counter := !counter + 1;
  Variable ("_G" ^ string_of_int c)

let freshen c =
  let vars = variables_of_clause c in
  let s = VarSet.fold (fun v s -> Substitution.add v (fresh()) s) vars Substitution.empty in
  substitute_in_clause s c

(*
let c = (rule (func "p" [var "X"; var "Y"; const "a"]) [func "q" [var "X"; const "b"; const "a"]])
let _ = print_endline (string_of_clause c)
let _ = print_endline (string_of_clause (freshen c))
*)

exception Not_unifiable

let rec unify_rec t1 t2 s = (* t1 is x and t2 is y and s is theta in the pseudocode *)
  let t1 = substitute_in_term s t1 in
  let t2 = substitute_in_term s t2 in
  match t1, t2 with
    |(Variable x, _) -> 
      if t1 = t2 then s
      else if (occurs_check t1 t2) then raise Not_unifiable
      else Substitution.add t1 t2 (Substitution.map (fun t -> substitute_in_term (Substitution.singleton t1 t2) t) s)
    |(_, Variable x) -> 
      if t1 = t2 then s
      else if (occurs_check t1 t2) then raise Not_unifiable
      else Substitution.add t2 t1 (Substitution.map (fun t -> substitute_in_term (Substitution.singleton t2 t1) t) s)
    |(Constant x, Constant y) -> 
      if t1 = t2 then s else raise Not_unifiable
    |(Function(h1, b1), Function(h2, b2)) -> 
      if h1 = h2 && List.length b1 = List.length b2 then (List.fold_left2 (fun s t1 t2 -> unify_rec t1 t2 s) s b1 b2) else raise Not_unifiable
    | _ -> raise Not_unifiable ;;

let unify t1 t2 = 
  unify_rec t1 t2 Substitution.empty ;;

(* Problem 5 *)

let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n-1) t ;;

let nondet_query program goal =
  let rec query program goal resolvent =
    match resolvent with
    | [] -> goal
    | resolvent -> 
    let randNum = Random.int (List.length resolvent) in 
    let a = List.nth resolvent randNum in (* random goal *)
    let a' = List.nth program (Random.int (List.length program)) in (* random rule *)
    let rs = remove_at randNum resolvent in 
    let a' = freshen a' in
    match a' with
    | Fact (head) ->
      (match unify head a with
      | exception (Not_unifiable) -> [] (* return something to loop as a sigal that unify fails *)
      | s -> (* succeeds so we can continue the recursion *)
      let goal' = List.map (fun x -> substitute_in_term s x) goal in (* apply substitution s to goals based on pseudocode *)
      let resolvent' = List.map (fun x -> substitute_in_term s x) rs in (* apply s to rs based on pseudocode *)
      query program goal' resolvent') (* recursion continues *)
    | Rule (head,body) ->
    (match unify head a with
      | exception (Not_unifiable) -> [] (* return something to loop as a sigal that unify fails *)
      | s -> (* succeeds so we can continue the recusion *)
      let rsNew = rs@body in
      let goal' = List.map (fun x -> substitute_in_term s x) goal in (* apply substitution s to goals based on pseudocode *)
      let resolvent' = List.map (fun x -> substitute_in_term s x) rsNew in (* apply s to rs based on pseudocode *)
      query program goal' resolvent')
      (*similar to the Fact case but need to add body to rs.*)
    in
    let rec loop () =
      let result = query program goal goal in (* correctly initialize the resolvent in the second parameter of query *)
      if result = [] then loop ()   (* failed so restart *)
      else result                            (* done *)
    in
    loop () ;;  

(* Problem Bonus *)

let det_query program goal =
  raise (Failure "Problem Bonus Not implemented")
