module type S = sig
  type t = private int

  val modulus : int

  val of_int : int -> t
  val to_int : t -> int

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val neg : t -> t
  val inv : t -> t  

  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
  val (/%) : t -> t -> t
  val (^%) : t -> int -> t
  val (~-%) : t -> t
  val (~/%) : t -> t
end

module Make (_ : sig val modulus : int end) : S
