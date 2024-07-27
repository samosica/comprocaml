(** Factorial.

    In 64-bit environment, return the correct value if an argument [n] 
    is less than or equal to 20; otherwise may not.

    Raise an exception if [n] is negative.
  *)
val fact : int -> int

(** The number of permutations.

    Return the correct value if it can be represented by [int].

    Return 0 if either of arguments is negative.
  *)
val perm : int -> int -> int

(** The number of combinations.

    Return the correct value if it can be represented by [int].

    Return 0 if either of arguments is negative.
  *)
val comb : int -> int -> int

(** The number of combinations with repetition.

    Return the correct value if it can be represented by [int].

    Return 0 if either of arguments is negative.
  *)
val comb_rep : int -> int -> int
