(** Apply a function to an element of an array *)
val replace : 'a array -> int -> ('a -> 'a) -> unit

(** Compute the next *subpermutation* of an array [a] in the lexicographic order.
    Just return [false] if [a] is the last subpermutation;
    otherwise modify [a] and return [true].

    The first (required) argument [n] specifies the type of subpermutations.
    A subpermutation of type ([l], [n]) is an array of size [l] such that
    the last [l - n] elements are sorted.

    This function allows duplicate elements.

    The time complexity is O(|a|).
  *)
val next_subpermutation : ?cmp:('a -> 'a -> int) -> int -> 'a array -> bool

(** Compute the next permutation of an array [a] in the lexicographic order.
    Just return [false] if [a] is the last permutation;
    otherwise modify [a] and return [true].

    This function allows duplicate elements.

    The time complexity is O(|a|).
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

(** Compute the next tuple of [from].
    Just return [false] if a given array [a] is the last tuple;
    otherwise modify [a] and return [true].
    The order of tuples is lexicographic one.

    [from] must be sorted in ascending order, and
    must not have duplicates.

    The time complexity is O(|a| + |from|).
  *)
val next_tuple : ?cmp:('a -> 'a -> int) -> from:'a array -> 'a array -> bool
