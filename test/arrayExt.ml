open Comprocaml

let%test "next_combination(5, 3)" =
  let from = [| 1; 2; 3; 4; 5 |] in
  let a = [| 1; 2; 3 |] in
  let rec loop k =
    k (Array.copy a);
    if ArrayExt.next_combination ~from a then loop k in
  let res =
    Iter.from_iter loop
    |> Iter.to_list in
  res = [
    [| 1; 2; 3 |];
    [| 1; 2; 4 |];
    [| 1; 2; 5 |];
    [| 1; 3; 4 |];
    [| 1; 3; 5 |];
    [| 1; 4; 5 |];
    [| 2; 3; 4 |];
    [| 2; 3; 5 |];
    [| 2; 4; 5 |];
    [| 3; 4; 5 |];
  ]

let%test "next_combination(26, 13)" =
  let n = 26 in
  let k = 13 in
  let from = Array.init n Fun.id in
  let a = Array.init k Fun.id in
  let count = ref 0 in
  let rec loop () =
    incr count;
    if ArrayExt.next_combination ~from a then loop () in
  loop();
  !count = 10_400_600

let%test "next_combination_rep(3, 2)" =
  let from = [| 1; 2; 3 |] in
  let a = [| 1; 1 |] in
  let rec loop k =
    k (Array.copy a);
    if ArrayExt.next_combination_rep ~from a then loop k in
  let res =
    Iter.from_iter loop
    |> Iter.to_list in
  res = [
    [| 1; 1 |];
    [| 1; 2 |];
    [| 1; 3 |];
    [| 2; 2 |];
    [| 2; 3 |];
    [| 3; 3 |];
  ]

let%test "next_combination_rep(7, 4)" =
  let n = 7 in
  let k = 4 in
  let from = Array.init n Fun.id in
  let a = Array.make k 0 in
  let count = ref 0 in
  let rec loop () =
    incr count;
    if ArrayExt.next_combination_rep ~from a then loop () in
  loop();
  !count = 210
