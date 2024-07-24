let divisible_count n d =
  let rec loop n c =
    if n mod d = 0 then
      loop (n / d) (c + 1)
    else
      c in
  loop n 0

let extgcd a b =
  let rec loop a b s t s' t'  =
    if b = 0 then
      (s, t, a)
    else
      loop b (a mod b) s' t' (s - (a / b) * s') (t - (a / b) * t') in
  loop a b 1 0 0 1
