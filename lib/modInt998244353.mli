type t = private int

(** = 998_244_353 *)
val modulus : int

(** Returns an integer such that of_int i ≡ i (mod 998,244,353) and
    0 <= of_int i < 998,244,353.
  *)
val of_int : int -> t
val to_int : t -> int

val zero : t
val one : t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val pow : t -> int -> t
val neg : t -> t
val inv : t -> t

val (+%) : t -> t -> t
val (-%) : t -> t -> t
val ( *% ) : t -> t -> t
val (/%) : t -> t -> t
val (^%) : t -> int -> t
val (~-%) : t -> t
val (~/%) : t -> t
