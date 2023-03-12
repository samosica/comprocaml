module type RingType = sig
  type t
  val zero : t
  val add : t -> t -> t
  val one : t
  val mul : t -> t -> t
end

module Matrix : sig
  module type S = sig
    type elt
    type t
    (* extended indexing operators *)
    (* (see https://v2.ocaml.org/manual/indexops.html#s:index-operators) *)
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

  module Make (R : RingType) : S with type elt = R.t
end = struct
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

  module Make (R : RingType) = struct
    type elt = R.t
    type t = elt array array

    let ( .@[] ) a (i, j) = a.(i).(j)
    let ( .@[]<- ) a (i, j) x = a.(i).(j) <- x

    let row_count a = Array.length a
    let column_count a = Array.length a.(0)

    let init ~row ~col f =
      assert (row > 0 && col > 0);
      Array.init row @@ fun i ->
        Array.init col @@ fun j ->
          f i j

    let zero ~row ~col =
      assert (row > 0 && col > 0);
      Array.make_matrix row col R.zero

    let one ~size =
      assert (size > 0);
      init ~row:size ~col:size (fun i j -> if i = j then R.one else R.zero)

    let add a b =
      assert (row_count a = row_count b);
      assert (column_count a = column_count b);
      init ~row:(row_count a) ~col:(column_count a) @@ fun i j ->
        R.add a.(i).(j) b.(i).(j)

    let mul a b =
      assert (column_count a = row_count b);
      init ~row:(row_count a) ~col:(column_count b) @@ fun i j ->
        Iter.(0 -- (column_count a - 1))
        |> Iter.fold (fun acc k -> R.add acc (R.mul a.(i).(k) b.(k).(j))) R.zero

    let expt a n =
      assert (row_count a = column_count a);
      let rec loop a n p =
        if n = 0 then
          p
        else if n mod 2 = 1 then
          loop (mul a a) (n / 2) (mul a p)
        else
          loop (mul a a) (n / 2) p in
      loop a n (one ~size:(row_count a))

    let of_array a =
      assert (Array.length a > 0);
      assert (Array.length a.(0) > 0);
      assert (Array.for_all (fun r -> Array.length r = Array.length a.(0)) a);
      a
  end
end