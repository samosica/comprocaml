let[@inline] replace a i f = a.(i) <- f a.(i)

let next_permutation ?(cmp = compare) a =
  let[@inline] rec f i =
    if i = 0 then i
    else if cmp a.(i - 1) a.(i) < 0 then i
    else f (i - 1) in
  let[@inline] rec g b j =
    if cmp b a.(j) < 0 then j
    else g b (j - 1) in
  let i = f (Array.length a - 1) in
  if i = 0 then false
  else begin
    let j = g a.(i - 1) (Array.length a - 1) in
    Base.Array.swap a (i - 1) j;
    for k = 0 to (Array.length a - i) / 2 - 1 do
      Base.Array.swap a (i + k) (Array.length a - 1 - k)
    done;
    true
  end

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
