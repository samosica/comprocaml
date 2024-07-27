val sum : int list -> int

val rep : int -> 'a list -> 'a list list

(** Returns the representation of an integer with a given base.
    The [i]-th element is the [i]-th digit.
  *)
val digits : base:int -> int -> int list
