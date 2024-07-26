open Comprocaml

let%test "next_subpermutation(4, 2)" =
  let n = 2 in
  let a = [| 1; 2; 3; 4 |] in
  let rec loop k =
    k (Array.copy a);
    if ArrayExt.next_subpermutation n a then loop k in
  let res =
    Iter.from_iter loop
    |> Iter.to_list in
  res = [
    [| 1; 2; 3; 4 |];
    [| 1; 3; 2; 4 |];
    [| 1; 4; 2; 3 |];
    [| 2; 1; 3; 4 |];
    [| 2; 3; 1; 4 |]; (* 5 *)
    [| 2; 4; 1; 3 |];
    [| 3; 1; 2; 4 |];
    [| 3; 2; 1; 4 |];
    [| 3; 4; 1; 2 |];
    [| 4; 1; 2; 3 |]; (* 10 *)
    [| 4; 2; 1; 3 |];
    [| 4; 3; 1; 2 |]; (* 12 *)
  ]

let%test "next_subpermutation(13, 7)" =
  let l = 13 in
  let n = 7 in
  let a = Array.init l Fun.id in
  let count = ref 0 in
  let rec loop () =
    incr count;
    if ArrayExt.next_subpermutation n a then loop () in
  loop();
  !count = 8_648_640

let%test "next_subpermutation with repetition" =
  let n = 3 in
  let l = [ 1; 1; 1; 2; 3; 3 ] in
  let a = Array.of_list l in
  let rec loop k =
    k (Array.copy a);
    if ArrayExt.next_subpermutation n a then loop k in
  let res =
    Iter.from_iter loop
    |> Iter.to_list in
  (* assume [from] is sorted *)
  let[@tail_mod_cons] rec remove ~from x =
    match from with
    | [] -> []
    | y :: from' ->
      if y = x then
        from'
      else
        y :: remove ~from:from' x in
  let f s =
    let rest = List.fold_left (fun acc i -> remove ~from:acc i) l s in
    Array.of_list (s @ rest) in
  let expected = [
    [ 1; 1; 1 ];
    [ 1; 1; 2 ];
    [ 1; 1; 3 ]; (* 3 *)

    [ 1; 2; 1 ];
    [ 1; 2; 3 ]; (* 5 *)

    [ 1; 3; 1 ];
    [ 1; 3; 2 ];
    [ 1; 3; 3 ]; (* 8 *)

    [ 2; 1; 1 ];
    [ 2; 1; 3 ]; (* 10 *)

    [ 2; 3; 1 ];
    [ 2; 3; 3 ]; (* 12 *)

    [ 3; 1; 1 ];
    [ 3; 1; 2 ];
    [ 3; 1; 3 ]; (* 15 *)

    [ 3; 2; 1 ];
    [ 3; 2; 3 ]; (* 17 *)

    [ 3; 3; 1 ];
    [ 3; 3; 2 ]; (* 19 *)
  ] in
  let expected = List.map f expected in
  res = expected

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

let%test "next_tuple(3, 4)" =
  let n = 3 in
  let k = 4 in
  let from = Array.init n Fun.id in
  let a = Array.make k 0 in
  let count = ref 0 in
  let rec loop () =
    incr count;
    if ArrayExt.next_tuple ~from a then loop () in
  loop();
  !count = 3 * 3 * 3 * 3

let%test "next_tuple(10, 0)" =
  let n = 10 in
  let from = Array.init n Fun.id in
  let a = [| |] in
  let count = ref 0 in
  let rec loop () =
    incr count;
    if ArrayExt.next_tuple ~from a then loop () in
  loop();
  !count = 1
