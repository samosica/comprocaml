module type S = sig
  type elt
  type t
  val ( .@[] ) : t -> int * int -> elt
  val ( .@[]<- ) : t -> int * int -> elt -> unit
  val row_count : t -> int
  val column_count : t -> int
  val init : row:int -> col:int -> (int -> int -> elt) -> t
  val zero : row:int -> col:int -> t
  val one : size:int -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val expt : t -> int -> t
  val of_array : elt array array -> t
end

module Make (R : Semiring.S) : S with type elt = R.t
