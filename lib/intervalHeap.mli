module type S = sig
  type elt
  type t

  (** Create a heap with a capacity of at least [cap] elements. *)
  val create : cap:int -> t

  (** Add a value to a heap (in-place).
      The given value is always added even if it already exists.

      The time complexity is amortized O(log n) where n is
      the number of elements.
    *)
  val add : elt -> t -> unit

  (** Remove the minimum value from a heap (in-place).

      Raise an exception if a heap is empty.

      The time complexity is O(log n) where n is the number of elements.
    *)  
  val remove_min : t -> unit

  (** Remove the maximum value from a heap (in-place).

      Raise an exception if a heap is empty.

      The time complexity is O(log n) where n is the number of elements.
    *)  
  val remove_max : t -> unit

  (** Return the minimum value of a heap.
      
      Raise an exception if a heap is empty.

      The time complexity is Θ(1).
    *)  
  val min_elt : t -> elt

  (** Return the minimum value of a heap if it is not empty;
      otherwise [None].

      The time complexity is Θ(1).
    *)
  val min_elt_opt : t -> elt option

  (** Return the maximum value of a heap.
      
      Raise an exception if a heap is empty.

      The time complexity is Θ(1).
    *) 
  val max_elt : t -> elt

  (** Return the maximum value of a heap if it is not empty;
      otherwise [None].

      The time complexity is Θ(1).
    *)
  val max_elt_opt : t -> elt option

  (** Return the number of elements in a heap.

      The time complexity is Θ(1).
    *)
  val cardinal : t -> int
  val is_empty : t -> bool
end

module Make (M : Set.OrderedType) : S with type elt = M.t
