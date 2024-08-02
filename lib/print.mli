(* Lists *)
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

(* Subarrays *)
(** Print a part of an array of an arbitrary type.
    A subarray of [a] from the [l]-th element (inclusive) to the [r]-th
    element (exclusive) is represented by a triple [(a, l, r)].

    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_subarray (fun ch -> Printf.fprintf ch "%d")) [| ... |]]
    - [Printf.printf "%a" (Print.output_subarray output_string) [| ... |]]
  *)
val output_subarray : 
  ?sep:string -> (out_channel -> 'a -> unit) -> out_channel ->
    ('a array * int * int) -> unit

(** Print a part of an array of integers.
    A subarray of [a] from the [l]-th element (inclusive) to the [r]-th
    element (exclusive) is represented by a triple [(a, l, r)].

    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_int_subarray()) [| ... |]]
    - [Printf.printf "%a" (Print.output_int_subarray ~sep:"..." ()) [| ... |]]
  *)
val output_int_subarray : 
  ?sep:string -> unit -> out_channel -> (int array * int * int) -> unit

(** Print a part of an array of strings.
    A subarray of [a] from the [l]-th element (inclusive) to the [r]-th
    element (exclusive) is represented by a triple [(a, l, r)].

    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_string_subarray()) [| ... |]]
    - [Printf.printf "%a" (Print.output_string_subarray ~sep:"..." ()) [| ... |]]
  *)
val output_string_subarray : 
  ?sep:string -> unit -> out_channel -> (string array * int * int) -> unit

val print_subarray : 
  ?sep:string -> ?end_:string -> (out_channel -> 'a -> unit) ->
    ('a array * int * int) -> unit

val print_int_subarray : 
  ?sep:string -> ?end_:string -> (int array * int * int) -> unit

val print_string_subarray : 
  ?sep:string -> ?end_:string -> (string array * int * int) -> unit

(* Arrays *)
(** Print an array of an arbitrary type.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_array (fun ch -> Printf.fprintf ch "%d")) [| ... |]]
    - [Printf.printf "%a" (Print.output_array output_string) [| ... |]]
  *)
val output_array : 
  ?sep:string -> (out_channel -> 'a -> unit) -> out_channel -> 'a array -> unit

(** Print an array of integers.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_int_array()) [| ... |]]
    - [Printf.printf "%a" (Print.output_int_array ~sep:"..." ()) [| ... |]]
  *)
val output_int_array : 
  ?sep:string -> unit -> out_channel -> int array -> unit

(** Print an array of strings.
    Elements are separated by [sep] (the default value is a single space).

    Usage:
    - [Printf.printf "%a" (Print.output_string_array()) [| ... |]]
    - [Printf.printf "%a" (Print.output_string_array ~sep:"..." ()) [| ... |]]
  *)
val output_string_array : 
  ?sep:string -> unit -> out_channel -> string array -> unit

val print_array : 
  ?sep:string -> ?end_:string -> (out_channel -> 'a -> unit) -> 'a array -> unit

val print_int_array : 
  ?sep:string -> ?end_:string -> int array -> unit

val print_string_array : 
  ?sep:string -> ?end_:string -> string array -> unit

(* Bits *)
(** Print an integer in binary representation.
    It starts with the most significant bit.
    The length is as short as possible but at least [w].

    Usage:
    - [Printf.printf "%a" (Print.output_bits ~w:0) 10] -> 1010
    - [Printf.printf "%a" (Print.output_bits ~w:7) 10] -> 0001010
  *)
val output_bits : w:int -> out_channel -> int -> unit

(*  Note: the parameter [w] is required but not optional because
    - if [w] is optional, [output_bits] have to take an extra parameter,
      like [output_int_list], to make sure of the lack
      (see https://ocaml.org/manual/5.2/lablexamples.html#s:optional-arguments), and
    - in most cases, [w] is not omitted.
*)
