let divisible_count n d =
  let rec loop n c =
    if n mod d = 0 then
      loop (n / d) (c + 1)
    else
      c in
  loop n 0