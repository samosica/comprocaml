(* TODO: Implement
 * - subsets (using decreasing order)
 * - subsets_inc (using increasing order)
 * - max_elt
 * - comparison operators (compare sets as integers)
 *)
(* TODO: Add assertions *)

module BitSet : sig
  type t
  val empty : t
  val singleton : int -> t
  val iota : int -> int -> t
  val (&&) : t -> t -> t
  val (||) : t -> t -> t
  val (--) : t -> t -> t
  val add : int -> t -> t
  val remove : int -> t -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val all_sets : unit -> t Iter.t
  val supersets : t -> t Iter.t
  val to_iter : t -> int Iter.t
  val min_elt : t -> int
  val to_int : t -> int
  val of_int : int -> t
  val mem : int -> t -> bool
end = struct
  type t = int
  let empty = 0
  let[@inline] singleton i = 1 lsl i
  let[@inline] iota l r = (1 lsl (r - l) - 1) lsl l
  let[@inline] (&&) s s' = s land s'
  let[@inline] (||) s s' = s lor s'
  let[@inline] (--) s s' = s land (lnot s')
  let[@inline] add i s = s lor (1 lsl i)
  let[@inline] remove i s = s -- singleton i
  let[@inline] is_empty s = s = 0
  let[@inline] cardinal s = Base.Int.popcount s
  let[@inline] rec supersets_aux s s' k =
    k s';
    if s' <> -1 then supersets_aux s ((s' + 1) lor s) k
  let[@inline] all_sets () = Iter.(append (0 -- max_int) (min_int --^ (-1)))
  let supersets s = Iter.from_iter (supersets_aux s s)
  let[@inline] min_elt s = Base.Int.ctz s
  let[@inline] rec to_iter_aux s k =
    if s <> 0 then begin
      let i = min_elt s in
      k i;
      to_iter_aux (remove i s) k
    end
  let to_iter s = Iter.from_iter (to_iter_aux s)
  let of_int s = s
  let to_int s = s
  let[@inline] mem i s = s lsr i > 0
end
