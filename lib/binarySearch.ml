let rec binary_search ~ok ~ng p =
  if abs (ok - ng) > 1 then
    let m = (ok + ng) / 2 in
    if p m then
      binary_search ~ok:m ~ng p
    else
      binary_search ~ok ~ng:m p
  else
    ok

let rec binary_search_float ~ok ~ng ~n p =
  if n > 0 then
    let m = (ok +. ng) *. 0.5 in
    if p m then
      binary_search_float ~ok:m ~ng ~n:(n - 1) p
    else
      binary_search_float ~ok ~ng:m ~n:(n - 1) p
  else
    ok
