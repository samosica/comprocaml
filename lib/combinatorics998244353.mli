(** Factorial modulo 998,244,353.

    Raise an exception if [n] is negative.
  *)
val fact : int -> ModInt998244353.t

(** [fact_memo n] is equivalent to [fact] except that:
    - this function precomputes from the factorial of 0 to the one of [n], and
    - stores previously computed values for reuse.
  *)
val fact_memo : int -> int -> ModInt998244353.t

(** The number of permutations modulo 998,244,353.

    Return 0 if either of arguments is negative.
  *)
val perm : int -> int -> ModInt998244353.t

(** The inverse version of [fact_memo]. *)
val inv_fact_memo : int -> int -> ModInt998244353.t

(** Equivalent to [perm] except for the use of [fact] and [inv_fact].
    
    This function is intended to combine with [fact_memo] and [inv_fact_memo].
  *)
val perm_from_fact :
  fact:(int -> ModInt998244353.t) ->
  inv_fact:(int -> ModInt998244353.t) ->
  int -> int -> ModInt998244353.t

(** The number of combinations modulo 998,244,353.

    Return 0 if either of arguments is negative.
  *)
val comb : int -> int -> ModInt998244353.t

(** Equivalent to [comb] except for the use of [fact] and [inv_fact].
    
    This function is intended to combine with [fact_memo] and [inv_fact_memo].
  *)
val comb_from_fact :
  fact:(int -> ModInt998244353.t) ->
  inv_fact:(int -> ModInt998244353.t) ->
  int -> int -> ModInt998244353.t

(** The number of combinations with repetition modulo 998,244,353.

    Return 0 if either of arguments is negative.
  *)
val comb_rep : int -> int -> ModInt998244353.t

(** Equivalent to [comb_rep] except for the use of [fact] and [inv_fact].
    
    This function is intended to combine with [fact_memo] and [inv_fact_memo].
  *)
val comb_rep_from_fact :
  fact:(int -> ModInt998244353.t) ->
  inv_fact:(int -> ModInt998244353.t) ->
  int -> int -> ModInt998244353.t
