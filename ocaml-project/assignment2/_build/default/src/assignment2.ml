(* This is the test driver of this assignment.
 * You should implement your code in lambda.ml.
 * Do not change implement your code here.
 *)

open Syntax
open Lambda

let rec alpha_equiv e1 e2 =
  match e1, e2 with
  | Var x, Var y -> x = y
  | App (a1,a2), App(b1,b2) ->alpha_equiv a1 b1 && alpha_equiv a2 b2
  | Lam (v1,e1), Lam(v2,e2) ->
      let v = fresh "v" in
      alpha_equiv (substitute e1 v1 (Var v)) (substitute e2 v2 (Var v))
  | _ -> false


(* Testing Problem 1: mem *)
let _ = assert (mem "b" ["a";"b";"c"] = true);
assert (mem "x" ["a";"b";"c"] = false)


(* Testing Problem 2: remove *)
let _ = assert (remove "b" ["a";"b";"c"] = ["a";"c"]);
assert (remove "x" ["a";"b";"c"] = ["a";"b";"c"])


(* Testing Problem 3: union *)
let _ = assert (union ["a"; "c"; "b"] ["d"; "b"; "x"; "a"] = ["a"; "b"; "c"; "d"; "x"])


(* Testing Problem 4: add *)
let _ = assert (add "b" ["a";"c"] = ["a";"b";"c"]);
assert (add "a" ["c"; "a"] = ["a";"c"])


(* Testing Problem 5: free_variables *)
let _ = assert (free_variables (parse_string "\\x.x") = []);
assert (free_variables (parse_string "\\x.y") = ["y"])


(* Testing Problem 6: substitute *)
let _ = assert (alpha_equiv
          (substitute (parse_string "\\y.x") "x" (parse_string "\\z.z w"))
          (parse_string "λy.λz.z w"));
assert (alpha_equiv
          (substitute (parse_string "\\x.x") "x" (parse_string "y"))
          (parse_string "λx.x"));
assert (alpha_equiv
          (substitute (parse_string "\\x.y") "y" (parse_string "x"))
          (parse_string "λx0.x"))


(* Testing Problem 7: reduce_cbv *)
let _ =
let expr, reduced = reduce_cbv (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λx.x) (λz.(λx.x) z)"));

let expr, reduced = reduce_cbv (parse_string "(λx.x) (λz.(λx.x) z)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "λz.(λx.x) z"));

let expr, reduced = reduce_cbv (parse_string "λz.(λx.x) z") in
assert (reduced = false &&
        alpha_equiv expr (parse_string "λz.(λx.x) z"));

let expr, reduced = reduce_cbv (parse_string "(λx.y) ((λx.x x) (λx.x x))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λx.y) ((λx.x x) (λx.x x))"));

let expr, reduced = reduce_cbv (parse_string "x y z") in
assert (reduced = false &&
        alpha_equiv expr (parse_string "x y z"))

(* Testing Problem 8: reduce_cbn *)
let _ =
let expr, reduced = reduce_cbn (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λx.x) (λz.(λx.x) z)"));

let expr, reduced = reduce_cbn (parse_string "(λx.x) (λz.(λx.x) z)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "λz.(λx.x) z"));

let expr, reduced = reduce_cbn (parse_string "λz.(λx.x) z") in
assert (reduced = false &&
        alpha_equiv expr (parse_string "λz.(λx.x) z"));

let expr, reduced = reduce_cbn (parse_string "(λx.y) ((λx.x x) (λx.x x))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "y"));

let expr, reduced = reduce_cbn (parse_string "(\\x.x x) ((\\z.z) y)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λz.z) y ((λz.z) y)"));

; let expr, reduced = reduce_cbn (parse_string "x y z") in
assert (reduced = false &&
        alpha_equiv expr (parse_string "x y z"))

(* Testing problem 9: reduce_normal *)
let _ =
let expr, reduced = reduce_normal (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λx.x) (λz.(λx.x) z)"));

let expr, reduced = reduce_normal (parse_string "(λx.x) (λz.(λx.x) z)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "λz.(λx.x) z"));

let expr, reduced = reduce_normal (parse_string "λz.(λx.x) z") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "λz. z"));

let expr, reduced = reduce_normal (parse_string "(λx.y) ((λx.x x) (λx.x x))") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "y"));

let expr, reduced = reduce_normal (parse_string "(\\x.x x) ((\\z.z) y)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λz.z) y ((λz.z) y)"));

let expr, reduced = reduce_normal (parse_string "f (\\x.x x) ((\\z.z) y)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "f (λx.x x) y"));

let expr, reduced = reduce_normal (parse_string "(\\x.(\\z.z) y) (\\x.x x)") in
assert (reduced = true &&
        alpha_equiv expr (parse_string "(λz.z) y"))

(* More tests *)
let _ =
let zero = parse_string "\\f.\\x. x" in
let one = parse_string "\\f.\\x. f x" in
let two = parse_string "\\f.\\x. f (f x)" in
let three = parse_string "\\f.\\x. f (f (f x))" in

let plus = parse_string "λm. λn. λs. λz. m s (n s z)" in
let mult = parse_string "λm. λn. λs. λz. m (n s) z" in

assert (alpha_equiv (eval_normal (App (App (plus, one), two))) three);
print_endline "";
assert (alpha_equiv (eval_normal (App (App (mult, one), three))) three);
print_endline "";
assert (alpha_equiv (eval_normal (App (App (mult, zero), three))) zero)
