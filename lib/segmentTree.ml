module type MonoidType = sig
  type t
  val one : t
  val mul : t -> t -> t
end

module Bit = struct
  let leftmost_bit n =
    let () = assert (n > 0) in
    let rec loop n w r =
      if w = 0 then
        r
      else if n lsr w > 0 then
        loop (n lsr w) (w / 2) (r + w)
      else
        loop n (w / 2) r in
    loop n 32 0
end

module SegmentTree : sig
  module type S = sig
    type elt
    type t

    val make : int -> t
    val set : int -> elt -> t -> unit
    val product : int -> int -> t -> elt
    val binary_search : (elt -> bool) -> t -> int
  end

  module Make (M : MonoidType) : S with type elt = M.t
end = struct
  module type S = sig
    type elt
    type t

    val make : int -> t
    val set : int -> elt -> t -> unit
    val product : int -> int -> t -> elt
    val binary_search : (elt -> bool) -> t -> int
  end

  module Make (M : MonoidType) = struct
    type elt = M.t
    type t = {
      leaf_count : int;
      prod : elt array;
    }
    
    let make n =
      let leaf_count =
        let rec loop m = if m < n then loop (m * 2) else m in
        loop 1 in
      {
        leaf_count = leaf_count;
        prod = Array.make (leaf_count * 2) M.one;
      }

    let rec recalc k t =
      if k > 1 then begin
        let p = k / 2 in
        t.prod.(p) <- M.mul t.prod.(p * 2 + 0) t.prod.(p * 2 + 1);
        recalc p t
      end

    let set k v t =
      let k = k + t.leaf_count in
      t.prod.(k) <- v;
      recalc k t

    let product l r t =
      let l = l + t.leaf_count in
      let r = r + t.leaf_count in
      let rec loop l r pl pr =
        if l < r then begin
          let (l', pl') =
            if l mod 2 > 0 then
              (l + 1, M.mul pl t.prod.(l))
            else
              (l, pl) in
          let (r', pr') =
            if r mod 2 > 0 then
              (r - 1, M.mul t.prod.(r - 1) pr)
            else
              (r, pr) in
          loop (l' / 2) (r' / 2) pl' pr'
        end else
          M.mul pl pr in
      loop l r M.one M.one

    let binary_search p t =
      let rec loop k i v w =
        if w = 1 then
          i
        else begin
          let v' = t.prod.(k * 2 + 0) in
          if p (M.mul v v') then
            loop (k * 2 + 0) i v (w / 2)
          else
            loop (k * 2 + 1) (i + w / 2) (M.mul v v') (w / 2)
        end in
      loop 1 0 M.one t.leaf_count
  end
end

module PrefixSumMonoid = struct
  (* prefix sum of interval, maximal prefix sum of subintervals *)
  type t = int * int
  let one = (0, -1_000_000_000_000_000_000)
  let mul (s, m) (s', m') = (s + s', max m (s + m'))
end

(* module SegTree = SegmentTree.Make (PrefixSumMonoid) *)