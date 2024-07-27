val divisor_pair_count : int -> int64

(** Sieve of Eratosthenes.
    [sieve n] returns two arrays [is_prime] and [largest_prime_factor] with size [n + 1].
    The i-th element of [is_prime] is whether i is a prime.
    The i-th element of [largest_prime_factor] is the largest prime factor of i if i >= 2;
    otherwise -1.
  *)
val sieve : int -> bool array * int array

val divisors : prime_factor:int array -> int -> int array
