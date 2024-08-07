open Comprocaml
open DisjointRangeSet

let%test "add(not disjoint)" =
  try
    let _ = empty |> add (1, 3) |> add (2, 4) in
    false
  with
  | Not_disjoint -> true
  | _ -> false

let%test "find_int_opt(empty)" =
  find_int_opt 1 empty = None

let%test "find_int_opt(in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_opt 6 s = Some (4, 8)

let%test "find_int_opt(not in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_opt 14 s = None

let%test "find_int_geq_opt(empty)" =
  find_int_geq_opt 1 empty = None

let%test "find_int_geq_opt(in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_geq_opt 6 s = Some (4, 8)

let%test "find_int_geq_opt(not in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_geq_opt 10 s = Some (15, 31)

let%test "find_int_geq_opt(not exist)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_geq_opt 100 s = None

let%test "find_int_gt_opt(empty)" =
  find_int_gt_opt 1 empty = None

let%test "find_int_gt_opt(in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_gt_opt 6 s = Some (15, 31)

let%test "find_int_gt_opt(not in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_gt_opt 10 s = Some (15, 31)

let%test "find_int_geq_opt(not exist)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (15, 31) in
  find_int_gt_opt 100 s = None

let%test "find_ints(empty)" =
  Iter.to_list (find_ints (1, 10) empty) = []

let%test "find_ints(nonempty)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (9, 13)
    |> add (17, 31) in
  Iter.to_list (find_ints (1, 10) s) = [(1, 2); (4, 8); (9, 13)]

let%test "find_ints(with min_int and max_int)" =
  let s =
    empty
    |> add (min_int, -1)
    |> add (0, max_int) in
  Iter.to_list (find_ints (-1, 1) s) = [(min_int, -1); (0, max_int)]

let%test "mem_int(empty)" =
  mem_int 3 empty = false

let%test "mem_int(in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (9, 13)
    |> add (17, 31) in
  mem_int 21 s = true

let%test "mem_int(not in)" =
  let s =
    empty
    |> add (1, 2)
    |> add (4, 8)
    |> add (9, 13)
    |> add (17, 31) in
  mem_int 3 s = false
