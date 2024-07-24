val (let>) : 'a Iter.t -> ('a -> 'b Iter.t) -> 'b Iter.t
val (let>!) : 'a Iter.t -> ('a -> unit) -> unit

val guard : bool -> (unit -> unit) -> unit
