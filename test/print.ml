open Comprocaml

let%expect_test "output_bits(0)" =
  Printf.printf "%a\n" (Print.output_bits ()) 0;
  [%expect{|
    0
  |}]

let%expect_test "output_bits(10)" =
  Printf.printf "%a\n" (Print.output_bits ()) 10;
  [%expect{|
    1010
  |}]

let%expect_test "output_bits(10) with width = 7" =
  Printf.printf "%a\n" (Print.output_bits ~width:7 ()) 10;
  [%expect{|
    0001010
  |}]
