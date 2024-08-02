type t = int

let empty = 0
let[@inline] is_empty s = s = empty
let[@inline] singleton i =
  assert (0 <= i && i < Sys.int_size);
  1 lsl i
let[@inline] inter s s' = s land s'
let[@inline] union s s' = s lor s'
let[@inline] diff s s' = s land (lnot s')

let[@inline] symdiff s s' = s lxor s'
let[@inline] (&:) s s' = inter s s'
let[@inline] (|:) s s' = union s s'
let[@inline] (-:) s s' = diff s s'
let[@inline] add i s =
  assert (0 <= i && i < Sys.int_size);
  s lor (1 lsl i)
let[@inline] remove i s =
  assert (0 <= i && i < Sys.int_size);
  s -: singleton i
let[@inline] range l r =
  assert (0 <= l && l < Sys.int_size);
  assert (0 <= r && r <= Sys.int_size);
  if l < r then
    (1 lsl (r - l) - 1) lsl l
  else
    0
let[@inline] mem i s =
  0 <= i && i < Sys.int_size && s lsr i land 1 > 0
let[@inline] cardinal s = Base.Int.popcount s
let[@inline] equal s s' = s = s'
let[@inline] compare s s' = Int.compare s s'
let[@inline] min_elt s =
  assert (s <> 0);
  Base.Int.ctz s
let[@inline] max_elt s =
  assert (s <> 0);
  Sys.int_size - 1 - Base.Int.clz s
let of_int s = s
let to_int s = s
let[@inline] rec to_iter_aux s k =
  if s <> 0 then begin
    let i = min_elt s in
    k i;
    to_iter_aux (remove i s) k
  end
let to_iter s = Iter.from_iter (to_iter_aux s)
let[@inline] all_sets () = Iter.(append (0 -- max_int) (min_int --^ (-1)))
let[@inline] rec subsets_aux s s' k =
  k s';
  if s' <> s then begin
    let i = min_elt (s' lxor s) in
    subsets_aux s ((s' lsr i + 1) lsl i) k
  end
let subsets ?(start = 0) s =
  assert (start land s = start);
  Iter.from_iter (subsets_aux s start)
let[@inline] rec subsets_dec_aux s s' k =
  k s';
  if s' <> 0 then subsets_dec_aux s ((s' - 1) land s) k
let subsets_dec s = Iter.from_iter (subsets_dec_aux s s)
let[@inline] rec supersets_aux s s' k =
  k s';
  if s' <> -1 then supersets_aux s ((s' + 1) lor s) k
let supersets s = Iter.from_iter (supersets_aux s s)

let[@inline] rec fixed_size_sets_aux ~n ~k s c =
  c s;
  if s < (1 lsl k - 1) lsl (n - k) then begin
    let i = s + (s land -s) in
    let s' = i lor (1 lsl (k - Base.Int.popcount i) - 1) in
    fixed_size_sets_aux ~n ~k s' c
  end
let fixed_size_sets ~n ~k = Iter.from_iter (fixed_size_sets_aux ~n ~k (1 lsl k - 1))
