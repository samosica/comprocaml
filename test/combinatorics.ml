open Comprocaml.Combinatorics

let%test "fact(10)" =
  fact 10 = 3_628_800

let%test "fact with negative integer" =
  try
    let _ = fact (-100) in
    false
  with
  | Assert_failure(_, _, _) -> true

let%test "perm(100, 3)" =
  perm 100 3 = 970_200

let%test "perm(10000, 0)" =
  perm 10000 0 = 1

let%test "perm(83, 102)" =
  perm 83 102 = 0

let%test "perm(0, 0)" =
  perm 0 0 = 1

let%test "perm with negative integer" =
  perm (-10) (-12) = 0

let%test "comb(100, 3)" =
  comb 100 3 = 161_700

let%test "comb(10000, 10000)" =
  comb 10000 10000 = 1

let%test "comb(10000, 0)" =
  comb 10000 0 = 1

let%test "comb(83, 102)" =
  comb 83 102 = 0

let%test "comb(0, 0)" =
  comb 0 0 = 1

let%test "comb with negative integer" =
  comb (-10) (-12) = 0

let%test "comb_rep(100, 3)" =
  comb_rep 100 3 = 171_700

let%test "comb_rep(10000, 0)" =
  comb_rep 10000 0 = 1

let%test "comb_rep(18, 6)" =
  comb_rep 18 6 = 100_947

let%test "comb_rep(0, 0)" =
  comb_rep 0 0 = 1

let%test "comb_rep with negative integer" =
  comb_rep (-10) (-12) = 0
