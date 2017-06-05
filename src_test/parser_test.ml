
let plus () =
  Alcotest.(check string) "same bools" "" ""

let test_set = [
  "parser snapshot tests", `Quick , plus ;
]

let tests = [
  "parser set", test_set
];;