let[@inline] replace a i f = a.(i) <- f a.(i)

let next_subpermutation ?(cmp = compare) n a =
  let l = Array.length a in
  let[@inline] rec f i =
    if i = 0 then i
    else if cmp a.(i - 1) a.(i) < 0 then i
    else f (i - 1) in
  let[@inline] rec g b j =
    if cmp b a.(j) < 0 then j
    else g b (j + 1) in
  if n < l && cmp a.(n - 1) a.(l - 1) < 0 then begin
    let j = g a.(n - 1) n in
    Base.Array.swap a (n - 1) j;
    true
  end else begin
    let i = f (n - 1) in
    if i = 0 then false
    else begin
      (* sort a[i, l) *)
      (* a[i, n) is decreasing and a[n, l) is increasing *)
      (* a.(n - 1) >= a.(l - 1) *)
      for k = 0 to (l - n) / 2 - 1 do
        Base.Array.swap a (n + k) (l - 1 - k)
      done;
      for k = 0 to (l - i) / 2 - 1 do
        Base.Array.swap a (i + k) (l - 1 - k)
      done;
      let j = g a.(i - 1) i in
      Base.Array.swap a (i - 1) j;
      true
    end
  end

let next_permutation ?(cmp = compare) a =
  next_subpermutation ~cmp (Array.length a) a

let next_combination ?(cmp = compare) ~from a =
  let len_from = Array.length from in
  let len_a = Array.length a in
  let[@inline] rec f i =
    if i < 0 then i
    else if cmp a.(i) from.(len_from - len_a + i) <> 0 then i
    else f (i - 1) in
  let[@inline] rec g b j =
    if cmp b from.(j) < 0 then j
    else g b (j + 1) in
  let i = f (len_a - 1) in
  if i < 0 then false
  else begin
    (* [a] is a subsequence of [from] so [from.(i) <= a.(i)] *)
    let j = g a.(i) (i + 1) in
    for k = i to len_a - 1 do
      a.(k) <- from.(j + k - i)
    done;
    true
  end

let next_combination_rep ?(cmp = compare) ~from a =
  let len_from = Array.length from in
  let len_a = Array.length a in
  let[@inline] rec f i =
    if i < 0 then i
    else if cmp a.(i) from.(len_from - 1) <> 0 then i
    else f (i - 1) in
  let[@inline] rec g b j =
    if cmp b from.(j) < 0 then j
    else g b (j + 1) in
  let i = f (len_a - 1) in
  if i < 0 then false
  else begin
    (* elements in [a] is drawn from [from] so [from.(0) <= a.(i)] *)
    let j = g a.(i) 1 in
    for k = i to len_a - 1 do
      a.(k) <- from.(j)
    done;
    true
  end
