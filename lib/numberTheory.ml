let divisor_pair_count n =
  let c = ref 0L in
  let rec loop x =
    match x * x <= n, n mod x = 0 with
    | true, true ->
      c := Int64.add !c (if x * x = n then 1L else 2L);
      loop (x + 1)
    | true, _ ->
      loop (x + 1)
    | _, _ -> () in
  loop 1; !c