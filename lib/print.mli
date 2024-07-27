(** Print a list of an arbitrary type.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_list (fun ch -> Printf.fprintf ch "%d")) [...]]
    - [Printf.printf "%a" (Print.output_list output_string) [...]]
  *)
val output_list : ?sep:string -> (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit

(** Print a list of integers.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_int_list()) [...]]
    - [Printf.printf "%a" (Print.output_int_list ~sep:"..." ()) [...]]
  *)
val output_int_list : ?sep:string -> unit -> out_channel -> int list -> unit

(** Print a list of strings.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_string_list()) [...]]
    - [Printf.printf "%a" (Print.output_string_list ~sep:"..." ()) [...]]
  *)
val output_string_list : ?sep:string -> unit -> out_channel -> string list -> unit

val print_int_list : ?sep:string -> ?end_:string -> int list -> unit

val print_string_list : ?sep:string -> ?end_:string -> string list -> unit

(** Print an integer in binary representation.
    The length is as short as possible,
    and if [width] is specified, it is at least [width].

    Usage:
    - [Printf.printf "%a" (Print.output_bits()) 10] -> 1010
    - [Printf.printf "%a" (Print.output_bits ~width:7 ()) 10] -> 0001010
  *)
val output_bits : ?width:int -> unit -> out_channel -> int -> unit
