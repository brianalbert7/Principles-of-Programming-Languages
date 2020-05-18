open Syntax
let parse_string = Lambda_parse.parse_string
let string_of_expr = string_of_expr

let r = ref 0

let fresh s =
  let v = !r in
  r := !r + 1;
  s ^ (string_of_int v)

(* Your implementation begins from here *)

let mem e l =
  List.fold_left(fun x a -> if a = e then true else x) false l ;;



let remove e l =
  List.fold_right(fun x a -> if x = e then a else x::a) l [] ;;



let rec remove_stutter l = 
  match l with
  | [] -> []
  | h::[] -> h::[]
  | h1::h2::t -> if h1 = h2 then (remove_stutter (h2::t)) else h1::(remove_stutter (h2::t)) ;;

let union l1 l2 =
  remove_stutter (List.sort String.compare (l1@l2)) ;;



let add e l =
  remove_stutter (List.sort compare (e::l)) ;;



let rec free_variables e =
  match e with
  | Var x -> [x]
  | App (e1, e2) -> union (free_variables e1) (free_variables e2)
  | Lam (x, e0) -> remove x (free_variables e0) ;;



let rec substitute expr a b =
  match expr with
  | Var x -> 
    if a = x then b 
    else expr
  | App (e1, e2) -> 
    App (substitute e1 a b, substitute e2 a b)
  | Lam (x, e0) -> 
    if a = x then expr
    else if not (mem x (free_variables b)) then
      Lam (x, substitute e0 a b)
    else 
      let z = fresh x in 
      let e0' = substitute e0 x (Var z) in 
      Lam (z, substitute e0' a b) ;;



let rec reduce_cbv e =
  match e with
  (Var _|Lam (_, _)) -> e, false
  | App (e1, e2) ->
    let e1', reduced = reduce_cbv e1 in
    if reduced then 
      (App (e1', e2), true)
    else 
      let e2', reduced = reduce_cbv e2 in
      if reduced then
        (App (e1, e2'), true) 
      else match e1 with
    | Lam (x,y) -> (substitute y x e2, true)
    | _ -> (e, false) 
    ;;



let rec reduce_cbn e =
  match e with
  | App (Lam (x, e), e2) -> (substitute e x e2, true)
  | App (e1, e2) ->
    let e1', reduced = reduce_cbn e1 in
    if reduced then 
      (App (e1', e2), true)
    else 
      let e2', reduced = reduce_cbn e2 in
      if reduced then (App (e1, e2'), true)
      else (App (e1, e2), false)
  | Lam (x, e) -> 
    (Lam (x, e), false)
  | _ -> (e, false) ;;




let rec reduce_normal e =
  match e with
  | App (Lam (x, e), e2) -> (substitute e x e2, true)
  | App (e1, e2) ->
    let e1', reduced = reduce_normal e1 in
    if reduced then 
      (App (e1', e2), true)
    else 
      let e2', reduced = reduce_normal e2 in
      if reduced then (App (e1, e2'), true)
      else (App (e1, e2), false)
  | Lam (x, e) -> 
    let e', reduced = reduce_normal e in
        if reduced then (Lam (x, e'), true)
        else (Lam (x, e'), false)
  | _ -> (e, false) ;;


(* Your implementation done here *)  

(* Debug your code by printing out evaluation results *)
let rec eval log depth reduce expr =
  if depth = 0 then failwith "non-termination?"
  else begin
    let expr', reduced = reduce expr in
    if not reduced then expr else begin
      if log then print_endline ("= " ^ (string_of_expr expr'));
      eval log (depth-1) reduce expr'
    end
  end
let eval_cbv = eval true 1000 reduce_cbv
let eval_cbn = eval true 1000 reduce_cbn
let eval_normal = eval true 1000 reduce_normal

(* To debug and observe the evaluation steps of your `reduce_cbv`, `reduce_cbn`
 * or `reduce_normal` implementation, use the following code.
 *
 *let _ = eval_cbv (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_cbn (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_normal (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *)
