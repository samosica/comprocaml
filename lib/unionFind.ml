module UnionFind : sig
  type t
  val make : int -> t
  val root : int -> t -> int
  val unite : int -> int -> t -> unit
end = struct
  type t = {
    parent : int array;
    rank : int array
  }

  let make n = {
    parent = Array.init n Fun.id;
    rank = Array.make n 0
  }

  let rec root v t =
    if v = t.parent.(v) then
      v
    else
      let r = root t.parent.(v) t in
      t.parent.(v) <- r;
      r

  let unite v w t =
    let v = root v t in
    let w = root w t in
    if v = w then
      ()
    else if t.rank.(v) > t.rank.(w) then
      t.parent.(w) <- v
    else begin
      t.parent.(v) <- w;
      if t.rank.(v) = t.rank.(w) then begin
        t.rank.(w) <- t.rank.(w) + 1
      end
    end
end
