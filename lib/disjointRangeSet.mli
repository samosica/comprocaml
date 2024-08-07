module Range : Set.OrderedType with type t = int * int

include Set.S with type elt := Range.t

exception Not_disjoint

(** Return the range containing a given value.
    If not present, return [None].
  *)
val find_int_opt : int -> t -> Range.t option

(** Return the range containing a given value.
    If not present, raise an exception.
  *)
val find_int : int -> t -> Range.t

(** Return the first (= smallest) range containing an integer
    greater than or equal to a given value.
    If not present, return [None].
  *)
val find_int_geq_opt : int -> t -> Range.t option

(** Return the first (= smallest) range containing an integer
    greater than or equal to a given value.
    If not present, raise an exception.
  *)
val find_int_geq : int -> t -> Range.t

(** Return the first (= smallest) range whose elements are
    greater than a given value.
    If not present, return [None].
  *)
val find_int_gt_opt : int -> t -> Range.t option

(** Return the first (= smallest) range whose elements are
    greater than a given value.
    If not present, raise an exception.
  *)
val find_int_gt : int -> t -> Range.t

(** Return ranges intersected with a given range in increasing order. *)
val find_ints : Range.t -> t -> Range.t Iter.t

(** [mem_int x s] checks if the ranges in [s] contain [x]. *)
val mem_int : int -> t -> bool

(** Add a range to a set.

    Raise [Not_disjoint] exception if the given range intersects with a one in the set.
  *)
val add : Range.t -> t -> t

(** Add a range to a set.

    Beware: in contrast to [add], [unsafe_add] does not raise any exceptions,
    even if the given range intersects with a one in the set.
  *)
val unsafe_add : Range.t -> t -> t
