(** Apply a function to an element of an array *)
let[@inline] replace a i f = a.(i) <- f a.(i)

(** Compute the next permutation of an array [a] in the lexicographic order.
    Just return [false] if [a] is the last permutation;
    otherwise modify [a] and return [true].
  *)
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
