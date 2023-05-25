module type MonoidType = sig
  type t
  val one : t
  val mul : t -> t -> t
end

(* a right action of a on t *)
module type ActionType = sig
  type t
  type a
  val act : t -> a -> t
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
    (M : MonoidType)
    (N : MonoidType)
    (_ : ActionType with type t = M.t and type a = N.t)
  : S with type elt = M.t and type act = N.t
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
    (M : MonoidType)
    (N : MonoidType)
    (Action : ActionType with type t = M.t and type a = N.t) =
  struct
    type elt = M.t
    type act = N.t
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
        prop = Array.make (leaf_count * 2) N.one;
      }

    let propagate k t =
      t.prod.(k) <- Action.act t.prod.(k) t.prop.(k);
      if k < t.leaf_count then begin
        t.prop.(k * 2 + 0) <- N.mul t.prop.(k * 2 + 0) t.prop.(k);
        t.prop.(k * 2 + 1) <- N.mul t.prop.(k * 2 + 1) t.prop.(k)
      end;
      t.prop.(k) <- N.one
    
    let propagate_topdown k t =
      for i = Bit.leftmost_bit k downto 1 do
        propagate (k lsr i) t
      done
    
    let rec recalc k t =
      if k > 1 then begin
        let p = k / 2 in
        let vl = Action.act t.prod.(p * 2 + 0) t.prop.(p * 2 + 0) in
        let vr = Action.act t.prod.(p * 2 + 1) t.prop.(p * 2 + 1) in
        t.prod.(p) <- M.mul vl vr;
        recalc p t
      end

    let set k v t =
      let k = k + t.leaf_count in
      propagate_topdown k t;
      t.prod.(k) <- v;
      t.prop.(k) <- N.one;
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
          if l mod 2 > 0 then t.prop.(l) <- N.mul t.prop.(l) f;
          if r mod 2 > 0 then t.prop.(r - 1) <- N.mul t.prop.(r - 1) f;
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
              (l + 1, M.mul pl (Action.act t.prod.(l) t.prop.(l)))
            else
              (l, pl) in
          let (r', pr') =
            if r mod 2 > 0 then
              (r - 1, M.mul (Action.act t.prod.(r - 1) t.prop.(r - 1)) pr)
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
          let v' = Action.act t.prod.(k * 2 + 0) t.prop.(k * 2 + 0) in
          if p (M.mul v v') then
            loop (k * 2 + 0) i v (w / 2)
          else
            loop (k * 2 + 1) (i + w / 2) (M.mul v v') (w / 2)
        end in
      loop 1 0 M.one t.leaf_count
  end
end

module IntAddMonoid = struct
  type t = int
  let one = 0
  let mul x y = x + y
end

module IntMaxMonoid = struct
  type t = int
  let one = min_int
  let mul x y = max x y
end

module IntAddAction = struct
  type t = int
  type a = int
  let act x a = x + a
end

(* module LazySegTree = LazySegmentTree.Make (IntMaxMonoid) (IntAddMonoid) (IntAddAction) *)
