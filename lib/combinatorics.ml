let fact n =
  let[@inline] rec fact_aux n p =
    if n <= 1 then p
    else fact_aux (n - 1) (p * n) in
  assert (n >= 0);
  fact_aux n 1

let perm n k =
  if n < 0 || k < 0 || n < k then
    0
  else
    let res = ref 1 in
    for i = n downto n - k + 1 do
      res := !res * i
    done;
    !res

let comb n k =
  if n < 0 || k < 0 || n < k then
    0
  else
    let res = ref 1 in
    (* avoid overflow *)
    let k = Int.min k (n - k) in
    for i = 1 to k do
      (* Note: after this update, !res = comb n i *)
      res := !res * (n + 1 - i) / i
    done;
    !res

let comb_rep n k =
  if n < 0 || k < 0 then
    0
  else if n = 0 && k = 0 then
    1
  else
    comb (n + k - 1) k
