let leftmost_bit n =
  let () = assert (n > 0) in
  let rec loop n w r =
    if w = 0 then
      r
    else if n lsr w > 0 then
      loop (n lsr w) (w / 2) (r + w)
    else
      loop n (w / 2) r in
  loop n 32 0
