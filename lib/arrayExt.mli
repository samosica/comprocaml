(** Apply a function to an element of an array *)
val replace : 'a array -> int -> ('a -> 'a) -> unit

(** Compute the next permutation of an array [a] in the lexicographic order.
    Just return [false] if [a] is the last permutation;
    otherwise modify [a] and return [true].
  *)
val next_permutation : ?cmp:('a -> 'a -> int) -> 'a array -> bool

(** Compute the next combination of an array [a] drawn from [from].
    Just return [false] if [a] is the last combination;
    otherwise modify [a] and return [true].
    The order of combinations is lexicographic one.

    Given two arrays must be sorted in ascending order, and
    [from] must not have duplicates.

    The time complexity is O(|a| + |from|).
  *)
val next_combination : ?cmp:('a -> 'a -> int) -> from:'a array -> 'a array -> bool

(** Compute the next combination *with repetition* drawn from [from].
    Just return [false] if a given array [a] is the last combination;
    otherwise modify [a] and return [true].
    The order of combinations is lexicographic one.

    Given two arrays must be sorted in ascending order, and
    [from] must not have duplicates.

    The time complexity is O(|a| + |from|).
  *)
val next_combination_rep : ?cmp:('a -> 'a -> int) -> from:'a array -> 'a array -> bool
