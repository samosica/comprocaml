val divisible_count : int -> int -> int

(** Extended Euclidean algorithm.
    Given a pair (a, b) of integers, returns a triple (s, t, g) of integers
    such that g is the GCD of a and b, and sa + tb = g.
  *)
val extgcd : int -> int -> int * int * int

val ceil_log2 : int -> int
