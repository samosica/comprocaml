module LazySegmentTree : sig
  module type S = sig
    type elt
    type act
    type t

    val make : int -> t
    val set : int -> elt -> t -> unit
    val apply_range : int -> int -> act -> t -> unit
    val product : int -> int -> t -> elt
    val binary_search : (elt -> bool) -> t -> int
  end

  module Make
    (M : Monoid.S)
    (A : Monoid.Action with type t = M.t)
  : S with type elt = M.t and type act = A.M.t
end = struct
  module type S = sig
    type elt
    type act
    type t

    val make : int -> t
    val set : int -> elt -> t -> unit
    val apply_range : int -> int -> act -> t -> unit
    val product : int -> int -> t -> elt
    val binary_search : (elt -> bool) -> t -> int
  end

  module Make
    (M : Monoid.S)
    (A : Monoid.Action with type t = M.t) =
  struct
    type elt = M.t
    type act = A.M.t
    type t = {
      leaf_count : int;
      prod : elt array;
      prop : act array;
    }
    
    let make n =
      let leaf_count =
        let rec loop m = if m < n then loop (m * 2) else m in
        loop 1 in
      {
        leaf_count = leaf_count;
        prod = Array.make (leaf_count * 2) M.one;
        prop = Array.make (leaf_count * 2) A.M.one;
      }

    let propagate k t =
      t.prod.(k) <- A.act t.prod.(k) t.prop.(k);
      if k < t.leaf_count then begin
        t.prop.(k * 2 + 0) <- A.M.mul t.prop.(k * 2 + 0) t.prop.(k);
        t.prop.(k * 2 + 1) <- A.M.mul t.prop.(k * 2 + 1) t.prop.(k)
      end;
      t.prop.(k) <- A.M.one
    
    let propagate_topdown k t =
      for i = Bit.leftmost_bit k downto 1 do
        propagate (k lsr i) t
      done
    
    let rec recalc k t =
      if k > 1 then begin
        let p = k / 2 in
        let vl = A.act t.prod.(p * 2 + 0) t.prop.(p * 2 + 0) in
        let vr = A.act t.prod.(p * 2 + 1) t.prop.(p * 2 + 1) in
        t.prod.(p) <- M.mul vl vr;
        recalc p t
      end

    let set k v t =
      let k = k + t.leaf_count in
      propagate_topdown k t;
      t.prod.(k) <- v;
      t.prop.(k) <- A.M.one;
      recalc k t

    let apply_range l r f t =
      let l = l + t.leaf_count in
      let r = r + t.leaf_count in
      let kl = l / (l land -l) in
      (* if r = 2^x then kr = 0 *)
      let kr = r / (r land -r) - 1 in
      let () =
        propagate_topdown kl t;
        if kr > 0 then propagate_topdown kr t in
      let rec loop l r =
        if l < r then begin
          if l mod 2 > 0 then t.prop.(l) <- A.M.mul t.prop.(l) f;
          if r mod 2 > 0 then t.prop.(r - 1) <- A.M.mul t.prop.(r - 1) f;
          let l' = if l mod 2 > 0 then l + 1 else l in
          let r' = if r mod 2 > 0 then r - 1 else r in
          loop (l' / 2) (r' / 2)
        end in
      loop l r;
      recalc kl t;
      recalc kr t
    
    let product l r t =
      let l = l + t.leaf_count in
      let r = r + t.leaf_count in
      let kl = l / (l land -l) in
      let kr = r / (r land -r) - 1 in
      let () =
        propagate_topdown kl t;
        if kr > 0 then propagate_topdown kr t in
      let rec loop l r pl pr =
        if l < r then begin
          let (l', pl') =
            if l mod 2 > 0 then
              (l + 1, M.mul pl (A.act t.prod.(l) t.prop.(l)))
            else
              (l, pl) in
          let (r', pr') =
            if r mod 2 > 0 then
              (r - 1, M.mul (A.act t.prod.(r - 1) t.prop.(r - 1)) pr)
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
          propagate k t;
          let v' = A.act t.prod.(k * 2 + 0) t.prop.(k * 2 + 0) in
          if p (M.mul v v') then
            loop (k * 2 + 0) i v (w / 2)
          else
            loop (k * 2 + 1) (i + w / 2) (M.mul v v') (w / 2)
        end in
      loop 1 0 M.one t.leaf_count
  end
end

(* module LazySegTree = LazySegmentTree.Make (IntMaxMonoid) (IntAddAction) *)
