
let plus () =
  Alcotest.(check bool) "same bools" true false

let test_set = [
  "parser snapshot tests", `Quick , plus ;
]

let tests = [
  "parser set", test_set
];;