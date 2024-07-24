module type S = sig
  type t
  val one : t
  val mul : t -> t -> t
end

(** prefix sum of interval, maximal prefix sum of subintervals *)
module PrefixSumMonoid : S with type t = int * int

module IntPairMinMonoid : S with type t = int * int

module IntAddMonoid : S with type t = int
