type t

val empty : t
val is_empty : t -> bool  
val singleton : int -> t
val inter : t -> t -> t
val union : t -> t -> t
val diff : t -> t -> t
val symdiff : t -> t -> t
val (&:) : t -> t -> t
val (|:) : t -> t -> t
val (-:) : t -> t -> t
val add : int -> t -> t
val remove : int -> t -> t

(** [range l r] is the range from [l] (inclusive) to [r] (exclusive).

    If [l] >= [r], return the empty set.
 *)
val range : int -> int -> t
val mem : int -> t -> bool
val cardinal : t -> int

(** Equality check of bitsets. *)
val equal : t -> t -> bool

(** Compare bitsets in their integer representations. *)
val compare : t -> t -> int
val min_elt : t -> int
val max_elt : t -> int
val of_int : int -> t
val to_int : t -> int
val to_iter : t -> int Iter.t
val all_sets : unit -> t Iter.t

(**
    Enumerate the subsets of a given set from [start] in increasing order.
    The default value of [start] is the empty set.
  *)
val subsets : ?start:t -> t -> t Iter.t

(** Enumerate the subsets of a given set in decreasing order. *)
val subsets_dec : t -> t Iter.t

val supersets : t -> t Iter.t

(** Enumerate sets of integers less than [n] with [k] elements.
    They are in the increasing order of their integer representations.
  *)
val fixed_size_sets : n:int -> k:int -> t Iter.t
