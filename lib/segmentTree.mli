module type S = sig
  type elt
  type t

  val make : int -> t
  val at : int -> t -> elt
  val set : int -> elt -> t -> unit
  val update : f:(elt -> elt) -> int -> t -> unit
  val product : int -> int -> t -> elt
  val binary_search : (elt -> bool) -> t -> int
end

module Make (M : Monoid.S) : S with type elt = M.t
