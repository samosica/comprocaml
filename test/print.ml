open Comprocaml

let%expect_test "output_list(empty)" =
  Printf.printf "%a\n" (Print.output_list output_char) [];
  [%expect{|
  |}]

let%expect_test "output_list(nonempty) with sep" =
  Printf.printf "%a\n" (Print.output_list ~sep:", " output_char) ['a'; 'b'; 'c'];
  [%expect{|
    a, b, c
  |}]

let%expect_test "output_int_list" =
  Printf.printf "%a\n" (Print.output_int_list()) [1; 33; 100];
  [%expect{|
    1 33 100
  |}]

let%expect_test "output_string_list" =
  Printf.printf "%a\n" (Print.output_string_list()) ["子"; "丑"; "寅"];
  [%expect{|
    子 丑 寅
  |}]

let%expect_test "print_int_list" =
  Print.print_int_list [1; 33; 100];
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    1 33 100
    !
  |}]

let%expect_test "output_string_list" =
  Print.print_string_list ["子"; "丑"; "寅"];
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    子 丑 寅
    !
  |}]

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
