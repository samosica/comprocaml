module type S = sig
  type t
  val zero : t
  val add : t -> t -> t
  val one : t
  val mul : t -> t -> t
end
