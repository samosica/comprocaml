val print_int_list : int list -> unit

(** Print an integer in binary representation.
    The length is as short as possible,
    and if [width] is specified, it is at least [width].

      Usage:
    - [Printf.printf "%a" (Print.output_bits()) 10] -> 1010
    - [Printf.printf "%a" (Print.output_bits ~width:7 ()) 10] -> 0001010
  *)
val output_bits : ?width:int -> unit -> out_channel -> int -> unit
