open OUnit2
open Asn3.Assignment3
open TestUtils

let test_assignment_1_1 ctxt =
  let dll = dll_of_list [] in
  assert_equal true @@ (is_empty dll)

let test_assignment_1_2 ctxt =
  let dll = dll_of_list [1;2;3] in
  assert_equal true @@ (not(is_empty dll))

let test_assignment_1_3 ctxt =
    let dll = dll_of_list [1;2;3] in
    let s = ref 0 in
    let _ = iter dll (fun c -> s := !s + c.content) in
    assert_equal 6 @@ (!s)

let test_assignment_1_4 ctxt =
  let dll = dll_of_list [1;2;3] in
  let n1 = match !dll with
    | Some n1 -> n1
    | None -> failwith "impossible" in
  let n2 = match n1.next with
    | Some n2 -> n2
    | None -> failwith "impossible" in
  let _ = remove dll n2 in
  let n3 = match n1.next with
    | Some n3 -> n3
    | None -> failwith "impossible" in
  assert_equal 3 @@ (n3.content)

let test_assignment_1_5 ctxt =
  let dll = dll_of_list [1;2;3] in
  let l = list_of_dll dll in
  assert_equal l @@ [1;2;3]

let test_assignment_1_6 ctxt =
  let l = dll_of_list [1;2;3] in
  let _ = assert_equal 3 @@ (length l) in
  let _ = duplicate l in
  assert_equal 6 @@ (length l)

let test_assignment_1_7 ctxt =
  let dll = dll_of_list [1;2;3] in
  let _ = assert_equal 3 @@ (length dll) in
  let _ = duplicate dll in
  let _ = assert_equal 6 @@ (length dll) in
  assert_equal [1;1;2;2;3;3] @@ (list_of_dll dll)

let test_assignment_1_8 ctxt =
  let l = dll_of_list [1;2;3;4;5] in
  let _ = reverse l in
  assert_equal ([5;4;3;2;1]) @@ (list_of_dll l)

let test_assignment_2_1 ctxt =
  assert_equal "[|1;2;3|]" @@ (SerializableIntArray.string_of_t [|1;2;3|])

let test_assignment_2_2 ctxt =
  assert_equal "[|[|1|];[|2;3|];[|4;5;6|]|]" @@ SerializableIntArrayArray.string_of_t [|[|1|]; [|2;3|]; [|4;5;6|]|]

let test_assignment_2_3 ctxt =
  assert_equal [1;2;3;4;5;6] @@ SerializableIntArrayArray.fold (fun xs x -> xs @ [x]) [] [|[|1|]; [|2;3|]; [|4;5;6|]|]

let test_assignment_2_4 ctxt =
  assert_equal "[|[7;8;9];[10;11;12];[13]|]" @@ SerializableIntListArray.string_of_t [|[7;8;9];[10;11;12];[13]|]

let test_assignment_2_5 ctxt =
  assert_equal 70 @@ SerializableIntListArray.fold (+) 0 [|[7;8;9];[10;11;12];[13]|]

let test_assignment_2_6 ctxt =
  assert_equal "[[|7;8;9|];[|10;11;12|];[|13|]]" @@ SerializableIntArrayList.string_of_t [[|7;8;9|];[|10;11;12|];[|13|]]

let test_assignment_2_7 ctxt =
  assert_equal 70 @@ SerializableIntArrayList.fold (+) 0 [[|7;8;9|];[|10;11;12|];[|13|]]

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
    "assignment_2_1" >:: test_assignment_2_1;
    "assignment_2_2" >:: test_assignment_2_2;
    "assignment_2_3" >:: test_assignment_2_3;
    "assignment_2_4" >:: test_assignment_2_4;
    "assignment_2_5" >:: test_assignment_2_5;
    "assignment_2_6" >:: test_assignment_2_6;
    "assignment_2_7" >:: test_assignment_2_7;
  ]

let _ = run_test_tt_main suite
