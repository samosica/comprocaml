module UnionFind : sig
  type t
  val make : int -> t
  val root : int -> t -> int
  val is_root : int -> t -> bool
  val unite : int -> int -> t -> unit
  val size : int -> t -> int
end = struct
  type t = {
    parent : int array;
    rank : int array;
    size : int array;
  }

  let make n = {
    parent = Array.init n Fun.id;
    rank = Array.make n 0;
    size = Array.make n 1;
  }

  let rec root v t =
    if v = t.parent.(v) then
      v
    else
      let r = root t.parent.(v) t in
      t.parent.(v) <- r;
      r

  let is_root v t = v = root v t

  let unite v w t =
    let v = root v t in
    let w = root w t in
    if v = w then
      ()
    else if t.rank.(v) > t.rank.(w) then begin
      t.parent.(w) <- v;
      t.size.(v) <- t.size.(v) + t.size.(w)
    end else begin
      t.parent.(v) <- w;
      t.size.(w) <- t.size.(w) + t.size.(v);
      if t.rank.(v) = t.rank.(w) then begin
        t.rank.(w) <- t.rank.(w) + 1
      end
    end

  let size v t = t.size.(root v t)
end
