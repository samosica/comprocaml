open ModInt998244353

let fact n =
  let[@inline] rec fact_aux n p =
    if n <= 1 then p
    else fact_aux (n - 1) (p *% of_int n) in
  assert (n >= 0);
  fact_aux n one

let fact_memo n =
  assert (n >= 0);
  let memo = ref [| one |] in
  let extend s' =
    let s = Array.length !memo in
    assert (s < s');
    let memo' = Array.init s' (fun i ->
      if i < s then !memo.(i) else zero
    ) in
    for i = s to s' - 1 do
      memo'.(i) <- memo'.(i - 1) *% of_int i
    done;
    memo := memo' in
  if n > 0 then extend (n + 1);
  (fun i ->
    if i >= Array.length !memo then extend (i + 1);
    !memo.(i)
  )

let perm n k =
  if n < 0 || k < 0 || n < k then
    zero
  else
    let res = ref one in
    for i = n downto n - k + 1 do
      res := !res *% of_int i
    done;
    !res

let inv_fact_memo n =
  assert (n >= 0);
  let memo = ref [| one |] in
  let extend s' =
    let s = Array.length !memo in
    assert (s < s');
    let memo' = Array.init s' (fun i ->
      if i < s then !memo.(i) else zero
    ) in
    memo'.(s' - 1) <- ~/% (~/% (memo'.(s - 1)) *% perm (s' - 1) (s' - s));
    for i = s' - 2 downto s do
      memo'.(i) <- memo'.(i + 1) *% of_int (i + 1)
    done;
    memo := memo' in
  if n > 0 then extend (n + 1);
  (fun i ->
    if i >= Array.length !memo then extend (i + 1);
    !memo.(i)
  )

let perm_from_fact ~fact ~inv_fact n k = 
  if n < 0 || k < 0 || n < k then
    zero
  else
    fact n *% inv_fact (n - k)

let comb n k =
  if n < 0 || k < 0 || n < k then
    zero
  else
    let res = ref one in
    let k = Int.min k (n - k) in
    for i = 1 to k do
      (* Note: after this update, !res = comb n i *)
      res := !res *% of_int (n + 1 - i) /% of_int i
    done;
    !res

let comb_from_fact ~fact ~inv_fact n k =
  if n < 0 || k < 0 || n < k then
    zero
  else
    fact n *% inv_fact k *% inv_fact (n - k)

let comb_rep n k =
  if n < 0 || k < 0 then
    zero
  else if n = 0 && k = 0 then
    one
  else
    comb (n + k - 1) k

let comb_rep_from_fact ~fact ~inv_fact n k =
  if n < 0 || k < 0 then
    zero
  else if n = 0 && k = 0 then
    one
  else
    comb_from_fact ~fact ~inv_fact (n + k - 1) k
