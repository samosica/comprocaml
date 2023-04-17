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

(** The refined version of Iter.int_range_by.
    [refined_int_range_by ~step l r] returns an iterator iterating over the
    intersection of the set of integers between l and r (inclusive) and the set
    of integers represented in the form l + step * i (where i is a nonnegative
    integer).
    [refined_int_range_by] does not always return the same iterator as
    [Iter.int_range_by]. For instance, step = 2, l = 11, and r = 10.
  *)
  let refined_int_range_by ~step l r =
    if r - l < 0 && step > 0 || r - l > 0 && step < 0 then
      Iter.empty
    else
      Iter.int_range_by ~step l r

(** Sieve of Eratosthenes.
    [sieve n] returns two arrays [is_prime] and [largest_prime_factor] with size [n + 1].
    The i-th element of [is_prime] is whether i is a prime.
    The i-th element of [largest_prime_factor] is the largest prime factor of i if i >= 2;
    otherwise -1.
  *)
let sieve n =
  assert (n >= 0);
  let is_prime = Array.make (n + 1) true in
  let largest_prime_factor = Array.make (n + 1) 0 in
  let () =
    is_prime.(0) <- false;
    largest_prime_factor.(0) <- -1 in
  let () =
    if n >= 1 then begin
      is_prime.(1) <- false;
      largest_prime_factor.(1) <- -1
    end in
  let () =
    if n >= 2 then begin
      largest_prime_factor.(2) <- 2;
      refined_int_range_by ~step:2 4 n |> Iter.iter @@ fun i ->
        is_prime.(i) <- false;
        largest_prime_factor.(i) <- 2
    end in
  let () =
    refined_int_range_by ~step:2 3 n |> Iter.iter @@ fun i ->
      if is_prime.(i) then begin
        largest_prime_factor.(i) <- i;
        refined_int_range_by ~step:i (2 * i) n |> Iter.iter @@ fun j ->
          is_prime.(j) <- false;
          largest_prime_factor.(j) <- i
      end in
  (is_prime, largest_prime_factor)

let divisors ~prime_factor n =
  assert (n + 1 <= Array.length prime_factor);
  let ds = [| 1 |] in
  let rec loop n ds =
    if n <= 1 then
      ds
    else
      let p = prime_factor.(n) in
      let c = divisible_count n p in
      let pl = Array.length ds in
      let ds' = Array.make (pl * (c + 1)) 0 in
      let pow_p = ref 1 in
      let () =
        for i = 0 to c do
          for j = 0 to pl - 1 do
            ds'.(i * pl + j) <- ds.(j) * !pow_p
          done;
          if i < c then pow_p := !pow_p * p
        done in
      loop (n / !pow_p) ds' in
  loop n ds