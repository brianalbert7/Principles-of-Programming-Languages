open OUnit2
open Asn1.Assignment1
open TestUtils

let test_assignment_1_1 ctxt =
  assert_equal [3;3;4;5;5] @@ (cond_dup [3;4;5] (fun x -> x mod 2 = 1))
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal [] @@ (cond_dup [] (fun x -> x mod 2 = 1));
  assert_equal [1;2;2;3;4;4;5] @@ (cond_dup [1;2;3;4;5] (fun x -> x mod 2 = 0))
  (* END HIDDEN TESTS *)

let test_assignment_1_2 ctxt =
  assert_equal 50 @@ (n_times((fun x-> x+1), 50, 0))
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal 1 @@ (n_times ((fun x->x+1), 0, 1));
  assert_equal 100 @@ (n_times((fun x-> x+2), 50, 0))
  (* END HIDDEN TESTS *)

let test_assignment_1_3 ctxt =
  assert_equal [2;3;4;5] @@ (range 2 5)
  (* BEGIN HIDDEN TESTS *)
  ;assert_equal [0] @@ (range 0 0);
  assert_equal true @@ (try range 1 0 = [] with IncorrectRange -> true)
  (* END HIDDEN TESTS *)

let test_assignment_1_4 ctxt =
  assert_equal [5;7] @@ (zipwith (+) [1;2;3] [4;5])
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal [(1,5); (2,6); (3,7)] @@ (zipwith (fun x y -> (x,y)) [1;2;3;4] [5;6;7])
  (* END HIDDEN TESTS *)

let test_assignment_1_5 ctxt =
  assert_equal [[1];[2];[3];[4]] @@ (buckets (=) [1;2;3;4]);
  assert_equal [[1];[2;2];[3;3;3];[4;4;4]] @@ (buckets (=) [1;2;3;4;2;3;4;3;4]);
  assert_equal [[1;4];[2;5];[3;6]] @@ (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6])
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal [[1; 3; 5]; [2; 4; 6]] @@ (buckets (fun x y -> (=) (x mod 2) (y mod 2)) [1;2;3;4;5;6])
  (* END HIDDEN TESTS *)

let test_assignment_1_6 ctxt =
  assert_equal [1; 2; 3; 1; 4; 2] @@ (remove_stutter [1;2;2;3;1;1;1;4;4;2;2])
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal [] @@ (remove_stutter []);
  assert_equal [1] @@ (remove_stutter [1;1;1;1;1]);
  assert_equal [1;2] @@ (remove_stutter [1;1;1;1;1;2])
  (* END HIDDEN TESTS *)

let test_assignment_1_7 ctxt =
  assert_equal [1;2;3;4] @@ (flatten ([[1;2];[3;4]]))
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal [1;2;3;4] @@ (flatten ([[1;2];[];[3;4];[]]))
  (* END HIDDEN TESTS *)

let test_assignment_1_8 ctxt =
  assert_equal [1;2;3] @@ (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))))
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal 6 @@ (fold_inorder (fun acc x -> x + acc) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))))
  ; assert_equal 3 @@ (fold_inorder (fun acc x -> if acc < x then x else failwith "acc > x") 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))))
  (* END HIDDEN TESTS *)

let test_assignment_1_9 ctxt =
  assert_equal 12586269025 @@ (fib_tailrec 50)
  (* BEGIN HIDDEN TESTS *)
  ; assert_equal 2880067194370816120 @@ (fib_tailrec 90)
  ; assert_equal 0 @@ (fib_tailrec 0)
  (* END HIDDEN TESTS *)

let suite =
  "public" >::: [
    "assignment_1_1" >:: test_assignment_1_1;
    "assignment_1_2" >:: test_assignment_1_2;
    "assignment_1_3" >:: test_assignment_1_3;
    "assignment_1_4" >:: test_assignment_1_4;
    "assignment_1_5" >:: test_assignment_1_5;
    "assignment_1_6" >:: test_assignment_1_6;
    "assignment_1_7" >:: test_assignment_1_7;
    "assignment_1_8" >:: test_assignment_1_8;
    "assignment_1_9" >:: test_assignment_1_9;
  ]

let _ = run_test_tt_main suite