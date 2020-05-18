open Assignment3

let test_assignment_1_1 ctxt =
  let dll = dll_of_list [] in
  assert (is_empty dll)

let test_assignment_1_2 ctxt =
  let dll = dll_of_list [1;2;3] in
  assert (not(is_empty dll))

let test_assignment_1_3 ctxt =
    let dll = dll_of_list [1;2;3] in
    let s = ref 0 in
    let _ = iter dll (fun c -> s := !s + c.content) in
    assert (!s = 6)

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
  assert (n3.content = 3)

let test_assignment_1_5 ctxt =
  let dll = dll_of_list [1;2;3] in
  let l = list_of_dll dll in
  assert (l = [1;2;3])

let test_assignment_1_6 ctxt =
  let l = dll_of_list [1;2;3] in
  let _ = assert (3 = length l) in
  let _ = duplicate l in
  assert (length l = 6)

let test_assignment_1_7 ctxt =
  let dll = dll_of_list [1;2;3] in
  let _ = assert (3 = length dll) in
  let _ = duplicate dll in
  let _ = assert (6 = length dll) in
  assert ([1;1;2;2;3;3] = list_of_dll dll)

let test_assignment_1_8 ctxt =
  let l = dll_of_list [1;2;3;4;5] in
  let _ = reverse l in
  assert ([5;4;3;2;1] = list_of_dll l)

let test_assignment_2_1 ctxt =
  assert ("[|1;2;3|]" = SerializableIntArray.string_of_t [|1;2;3|])

let test_assignment_2_2 ctxt =
  assert ("[|[|1|];[|2;3|];[|4;5;6|]|]" = SerializableIntArrayArray.string_of_t [|[|1|]; [|2;3|]; [|4;5;6|]|])

let test_assignment_2_3 ctxt =
  assert ([1;2;3;4;5;6] = SerializableIntArrayArray.fold (fun xs x -> xs @ [x]) [] [|[|1|]; [|2;3|]; [|4;5;6|]|])

let test_assignment_2_4 ctxt =
  assert ("[|[7;8;9];[10;11;12];[13]|]" = SerializableIntListArray.string_of_t [|[7;8;9];[10;11;12];[13]|])

let test_assignment_2_5 ctxt =
  assert (70 = SerializableIntListArray.fold (+) 0 [|[7;8;9];[10;11;12];[13]|])

let test_assignment_2_6 ctxt =
  assert ("[[|7;8;9|];[|10;11;12|];[|13|]]" = SerializableIntArrayList.string_of_t [[|7;8;9|];[|10;11;12|];[|13|]])

let test_assignment_2_7 ctxt =
  assert (70 = SerializableIntArrayList.fold (+) 0 [[|7;8;9|];[|10;11;12|];[|13|]])

let _ =
test_assignment_1_1 ();
test_assignment_1_2 ();
test_assignment_1_3 ();
test_assignment_1_4 ();
test_assignment_1_5 ();
test_assignment_1_6 ();
test_assignment_1_7 ();
test_assignment_1_8 ();
test_assignment_2_1 ();
test_assignment_2_2 ();
test_assignment_2_3 ();
test_assignment_2_4 ();
test_assignment_2_5 ();
test_assignment_2_6 ();
test_assignment_2_7 ()

let _ = Printf.printf "All tests passed.\n"
