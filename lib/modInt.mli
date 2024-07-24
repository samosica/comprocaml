module type S = sig
  (* TODO: add modulus *)
  type t

  val of_int : int -> t
  val to_int : t -> int

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val inv : t -> t  

  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
  val (/%) : t -> t -> t
  val (~/%) : t -> t
  val (^%) : t -> int -> t
end

(* TODO: mo -> modulus ? *)
module Make (_ : sig val mo : int end) : S
