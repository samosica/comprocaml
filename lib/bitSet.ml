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
  val is_empty : t -> bool  
  val singleton : int -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val (&:) : t -> t -> t
  val (|:) : t -> t -> t
  val (-:) : t -> t -> t
  val add : int -> t -> t
  val remove : int -> t -> t
  val iota : int -> int -> t
  val mem : int -> t -> bool
  val cardinal : t -> int
  val min_elt : t -> int
  val of_int : int -> t
  val to_int : t -> int
  val to_iter : t -> int Iter.t
  val all_sets : unit -> t Iter.t

  (** Enumerate the subsets of a given set in increasing order. *)
  val subsets : t -> t Iter.t

  (** Enumerate the subsets of a given set in decreasing order. *)
  val subsets_dec : t -> t Iter.t

  val supersets : t -> t Iter.t
end = struct
  type t = int
  let empty = 0
  let[@inline] is_empty s = s = empty
  let[@inline] singleton i =
    assert (0 <= i && i < Sys.int_size);
    1 lsl i
  let[@inline] inter s s' = s land s'
  let[@inline] union s s' = s lor s'
  let[@inline] diff s s' = s land (lnot s')
  let[@inline] (&:) s s' = inter s s'
  let[@inline] (|:) s s' = union s s'
  let[@inline] (-:) s s' = diff s s'
  let[@inline] add i s =
    assert (0 <= i && i < Sys.int_size);
    s lor (1 lsl i)
  let[@inline] remove i s =
    assert (0 <= i && i < Sys.int_size);
    s -: singleton i
  let[@inline] iota l r =
    assert (0 <= l && l < Sys.int_size);
    assert (0 <= r && r <= Sys.int_size);
    if l < r then
      (1 lsl (r - l) - 1) lsl l
    else
      0
  let[@inline] mem i s =
    0 <= i && i < Sys.int_size && s lsr i > 0
  let[@inline] cardinal s = Base.Int.popcount s
  let[@inline] min_elt s =
    assert (s <> 0);
    Base.Int.ctz s
  let of_int s = s
  let to_int s = s
  let[@inline] rec to_iter_aux s k =
    if s <> 0 then begin
      let i = min_elt s in
      k i;
      to_iter_aux (remove i s) k
    end
  let to_iter s = Iter.from_iter (to_iter_aux s)
  let[@inline] all_sets () = Iter.(append (0 -- max_int) (min_int --^ (-1)))
  let[@inline] rec subsets_aux s s' k =
    k s';
    if s' <> s then begin
      let i = min_elt (s' lxor s) in
      subsets_aux s ((s' lsr i + 1) lsl i) k
    end
  let subsets s = Iter.from_iter (subsets_aux s 0)
  let[@inline] rec subsets_dec_aux s s' k =
    k s';
    if s' <> 0 then subsets_dec_aux s ((s' - 1) land s) k
  let subsets_dec s = Iter.from_iter (subsets_dec_aux s s)
  let[@inline] rec supersets_aux s s' k =
    k s';
    if s' <> -1 then supersets_aux s ((s' + 1) lor s) k
  let supersets s = Iter.from_iter (supersets_aux s s)
  let[@inline] all_sets () = Iter.(append (0 -- max_int) (min_int --^ (-1)))
end
