
let plus () =
  Alcotest.(check int) "same ints" 7 7

let test_set = [
  "token test first", `Quick , plus ;
]

let tests = [
  "token tests", test_set
];;