module type S = sig
  type t
  val one : t
  val mul : t -> t -> t
end

(** Right monoid actions. *)
module type Action = sig
  type t
  (** The type acted on by [A]. *)

  (** The monoid that acts on [t]. *)
  module M : S

  (** Right monoid action. *)
  val act : t -> M.t -> t
end

(** prefix sum of interval, maximal prefix sum of subintervals *)
module PrefixSumMonoid : S with type t = int * int

module IntPairMinMonoid : S with type t = int * int

module IntAddMonoid : S with type t = int

module IntMaxMonoid : S with type t = int

module IntAddAction : Action with type t = int and module M = IntAddMonoid
