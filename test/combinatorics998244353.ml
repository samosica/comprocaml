open Comprocaml
open ModInt998244353
module CI = Comprocaml.Combinatorics
module CM = Comprocaml.Combinatorics998244353

let%test "fact" =
  for i = 1 to 20 do
    assert (of_int (CI.fact i) = CM.fact i)
  done;
  true

let%test "fact_memo" =
  let fact = CM.fact_memo 10 in
  for i = 1 to 20 do
    assert (of_int (CI.fact i) = fact i)
  done;
  true

let%test "perm" =
  for n = -1 to 20 do
    for k = -1 to 20 do
      assert (of_int (CI.perm n k) = CM.perm n k)
    done
  done;
  true

let%test "inv_fact_memo" =
  let inv_fact = CM.inv_fact_memo 10 in
  for i = 1 to 20 do
    assert (~/% (of_int (CI.fact i)) = inv_fact i)
  done;
  true

let%test "comb" =
  for n = -1 to 40 do
    for k = -1 to 40 do
      assert (of_int (CI.comb n k) = CM.comb n k)
    done
  done;
  true

let%test "comb_from_fact with *_memo" =
  let fact = CM.fact_memo 10 in
  let inv_fact = CM.inv_fact_memo 10 in
  for n = -1 to 40 do
    for k = -1 to 40 do
      assert (of_int (CI.comb n k) = CM.comb_from_fact ~fact ~inv_fact n k)
    done
  done;
  true

let%test "comb_rep" =
  for n = -1 to 20 do
    for k = -1 to 20 do
      assert (of_int (CI.comb_rep n k) = CM.comb_rep n k)
    done
  done;
  true
