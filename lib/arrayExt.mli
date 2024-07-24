(** Apply a function to an element of an array *)
val replace : 'a array -> int -> ('a -> 'a) -> unit

(** Compute the next permutation of an array [a] in the lexicographic order.
    Just return [false] if [a] is the last permutation;
    otherwise modify [a] and return [true].
  *)
val next_permutation : ?cmp:('a -> 'a -> int) -> 'a array -> bool
