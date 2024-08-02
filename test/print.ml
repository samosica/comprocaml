open Comprocaml

(* Lists *)
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

let%expect_test "print_string_list" =
  Print.print_string_list ["子"; "丑"; "寅"];
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    子 丑 寅
    !
  |}]

(* Subarrays *)
let%expect_test "output_subarray(nonempty) with sep" =
  Printf.printf "%a\n"
    (Print.output_subarray ~sep:", " output_char)
    ([| 'a'; 'b'; 'c'; 'd' |], 1, 3);
  [%expect{|
    b, c
  |}]

let%expect_test "output_subarray(empty)" =
  Printf.printf "%a\n"
    (Print.output_subarray output_char)
    ([| 'a'; 'b'; 'c'; 'd' |], 3, 1);
  [%expect{|
  |}]

let%expect_test "output_int_subarray" =
  Printf.printf "%a\n" (Print.output_int_subarray()) ([| 0; 1; 2; 3; 4 |], 1, 4);
  [%expect{|
    1 2 3
  |}]

let%expect_test "output_string_subarray" =
  Printf.printf "%a\n"
    (Print.output_string_subarray())
    ([| "子"; "丑"; "寅" |], 0, 2);
  [%expect{|
    子 丑
  |}]

let%expect_test "print_int_subarray" =
  Print.print_int_subarray ([| 1; 33; 108; 100 |], 0, 4);
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    1 33 108 100
    !
  |}]

let%expect_test "print_string_subarray" =
  Print.print_string_subarray ([| "子"; "丑"; "寅"; "卯"; "辰" |], 3, 5);
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    卯 辰
    !
  |}]

(* Arrays *)
let%expect_test "output_array(nonempty)" =
  Printf.printf "%a\n"
    (Print.output_array output_char)
    [| '3'; '2'; '1'; '0' |];
  [%expect{|
    3 2 1 0
  |}]

let%expect_test "output_array(empty)" =
  Printf.printf "%a\n"
    (Print.output_array output_char)
    [||];
  [%expect{|
  |}]

let%expect_test "print_int_array" =
  Print.print_int_array [| 1; 1; 2; 3; 5 |];
  print_string "!"; (* check a newline is inserted *)
  [%expect{|
    1 1 2 3 5
    !
  |}]

(* Bits *)
let%expect_test "output_bits(0)" =
  Printf.printf "%a\n" (Print.output_bits ~w:0) 0;
  [%expect{|
    0
  |}]

let%expect_test "output_bits(10)" =
  Printf.printf "%a\n" (Print.output_bits ~w:0) 10;
  [%expect{|
    1010
  |}]

let%expect_test "output_bits(10) with w = 7" =
  Printf.printf "%a\n" (Print.output_bits ~w:7) 10;
  [%expect{|
    0001010
  |}]

let%expect_test "output_bits_from_lsb(0)" =
  Printf.printf "%a\n" (Print.output_bits_from_lsb ~w:0) 0;
  [%expect{|
    0
  |}]

let%expect_test "output_bits_from_lsb(10)" =
  Printf.printf "%a\n" (Print.output_bits_from_lsb ~w:0) 10;
  [%expect{|
    0101
  |}]

let%expect_test "output_bits_from_lsb(10) with w = 7" =
  Printf.printf "%a\n" (Print.output_bits_from_lsb ~w:7) 10;
  [%expect{|
    0101000
  |}]
