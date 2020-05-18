let rec cond_dup l f =
  match l with
  | [] -> []
  | h::t -> if f h then h::h::(cond_dup t f) else h::(cond_dup t f) ;;

(* assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5]) *)

let rec n_times (f, n, v) =
  if n <= 0
  then v
  else n_times(f, n-1, f v) ;;

(* assert (n_times((fun x-> x+1), 50, 0) = 50) *)

exception IncorrectRange

let rec range num1 num2 =
  if num1 > num2
  then raise (Failure "IncorrectRange")
  else if num1 = num2 then [num1] 
  else num1 :: (range (num1+1) num2) ;;

(* assert (range 2 5 = [2;3;4;5]) *)

let rec zipwith f l1 l2 =
  match (l1, l2) with
  | ([],_) -> []
  | (_,[]) -> []
  | (h1::t1, h2::t2) -> (f h1 h2) :: (zipwith f t1 t2) ;;

(* assert (zipwith (+) [1;2;3] [4;5] = [5;7]) *)

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list ;;

let buckets p l =
  let rec find p_find h_find acc_find =
    match acc_find with 
    | [] -> [[h_find]]
    | h::t -> 
      match h with 
        | head::_ -> if p head h_find then rev((h_find::h))::t else h::(find p h_find t)
        | [] -> []
    in 
    let rec iterate p_iter l_iter acc_iter = 
      match l_iter with
      | [] -> acc_iter
      | hd::tl -> iterate p tl (find p hd acc_iter)
    in 
      match l with 
      | [] -> []
      | hd::tl -> iterate p tl [[hd]] ;;


(* assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]) *)
(* assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]) *)
(* assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]]) *)

let rec remove_stutter l = 
  match l with
  | [] -> []
  | h::[] -> h::[]
  | h1::h2::t -> if h1 = h2 then (remove_stutter (h2::t)) else h1::(remove_stutter (h2::t)) ;;

(* assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]) *)

let rec flatten l = 
  match l with
  | [] -> []
  | h::[] -> h
  | h::t -> h @ (flatten t) ;;

(* assert (flatten ([[1;2];[3;4]]) = [1;2;3;4]) *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec fold_inorder f acc t =
  match t with
  | Leaf -> acc
  | Node (l, x, r) -> fold_inorder f (f (fold_inorder f acc l) x) r ;;

(* assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]) *)

let fib_tailrec n =
  let rec fib x y z = 
    if x = n then y
    else (fib (x+1) z (y+z)) in (fib 0 0 1) ;;

(* assert (fib_tailrec 50 = 12586269025) *)
