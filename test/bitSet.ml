open Comprocaml

let%test "range(3, 6)" =
  let s = BitSet.range 3 6 in
  Iter.(0 -- (Sys.int_size - 1))
  |> Iter.for_all (fun i -> BitSet.mem i s = (3 <= i && i < 6))

let%test "range(8, 6)" =
  let s = BitSet.range 8 6 in
  Iter.(0 -- (Sys.int_size - 1))
  |> Iter.for_all (fun i -> BitSet.mem i s = false)

let%test "combinations(5, 3)" =
  let res =
    BitSet.combinations ~n:5 ~k:3
    |> Iter.map BitSet.to_int
    |> Iter.to_list in
  res = [
    0b00111;
    0b01011;
    0b01101;
    0b01110;
    0b10011; (* 5 *)
    0b10101;
    0b10110;
    0b11001;
    0b11010;
    0b11100; (* 10 *)
  ]

let%test "combinations(17, 8)" =
  let res =
    BitSet.combinations ~n:17 ~k:8
    |> Iter.length in
  res = 24_310

let%test "subsets_of_fullset(10)" =
  let count =
    BitSet.subsets_of_fullset 10
    |> Iter.length in
  count = 1024
