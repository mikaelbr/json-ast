(* A module with functions to test *)
module To_test = struct
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

(* The tests *)
let capit () =
  Alcotest.(check char) "Check A" 'A' 'a'

let plus () =
  Alcotest.(check int) "Sum equals to 7" 7 (To_test.plus [1;1;2;3])

let test_set = [
  "\xF0\x9F\x90\xAB  Capitalize", `Quick, capit;
  "Add entries"                 , `Slow , plus ;
]

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "test_1", test_set;
    "test_2", test_set;
  ]