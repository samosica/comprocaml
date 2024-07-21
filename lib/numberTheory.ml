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
      Iter.iterate ((+) 2) 4
      |> Iter.take_while ((>=) n)
      |> Iter.iter @@ fun i ->
        is_prime.(i) <- false;
        largest_prime_factor.(i) <- 2
    end in
  let () =
    Iter.iterate ((+) 2) 3
    |> Iter.take_while ((>=) n)
    |> Iter.iter @@ fun i ->
      if is_prime.(i) then begin
        largest_prime_factor.(i) <- i;
        Iter.iterate ((+) i) (2 * i)
        |> Iter.take_while ((>=) n)
        |> Iter.iter @@ fun j ->
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
      let c = Math.divisible_count n p in
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

let fact n =
  let[@inline] rec fact_aux n p =
    if n <= 1 then p
    else fact_aux (n - 1) (p * n) in
  assert (n >= 0);
  fact_aux n 1
