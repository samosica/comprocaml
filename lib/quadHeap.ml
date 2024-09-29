module type S = sig
  type elt
  type t

  val create : cap:int -> t
  val add : elt -> t -> unit
  val remove_min : t -> unit

  val min_elt : t -> elt
  val min_elt_opt : t -> elt option

  val cardinal : t -> int
  val is_empty : t -> bool
end

module Make (M : Set.OrderedType) : S with type elt = M.t = struct
  type elt = M.t
  type t = {
    mutable data : elt Base.Option_array.t;
    mutable count : int;
  }

  let[@inline] ( .+() ) a i = Base.Option_array.unsafe_get_some_exn a i
  let[@inline] ( .+()<- ) a i v = Base.Option_array.unsafe_set_some a i v

  let extend a =
    let l = Base.Option_array.length a in
    (* Note: if [l] = 1 + 4^1 + ... + 4^n, then [4 * l + 1] = 1 + 4^1 + ... + 4^{n+1}. *)
    Base.Option_array.init (4 * l + 1) ~f:(fun i ->
      if i < l then Base.Option_array.unsafe_get a i else None
    )
  let extend h = h.data <- extend h.data
  
  let[@inline] is_full h = h.count = Base.Option_array.length h.data

  let create ~cap =
    (* See the comment in [extend]. *)
    let rec compute_len l =
      if l < cap then compute_len (4 * l + 1) else l in
    { data = Base.Option_array.create ~len:(compute_len 1); count = 0 }

  let rec go_up i h =
    let p = (i - 1) / 4 in
    if i > 0 && M.compare h.data.+(i) h.data.+(p) < 0 then begin
      Base.Option_array.swap h.data i p;
      go_up p h
    end

  let add v h =
    if is_full h then extend h;
    h.data.+(h.count) <- v;
    h.count <- h.count + 1;
    go_up (h.count - 1) h

  let rec go_down i h =
    let c1 = i * 4 + 1 in
    let min_c =
      if c1 < h.count && M.compare h.data.+(c1) h.data.+(i) < 0 then
        c1
      else
        i in
    let min_c =
      if c1 + 1 < h.count && M.compare h.data.+(c1 + 1) h.data.+(min_c) < 0 then
        c1 + 1
      else
        min_c in
    let min_c = 
      if c1 + 2 < h.count && M.compare h.data.+(c1 + 2) h.data.+(min_c) < 0 then
        c1 + 2
      else
        min_c in
    let min_c = 
      if c1 + 3 < h.count && M.compare h.data.+(c1 + 3) h.data.+(min_c) < 0 then
        c1 + 3
      else
        min_c in
    if min_c <> i then begin
      Base.Option_array.swap h.data min_c i;
      go_down min_c h
    end

  let remove_min h =
    Base.Option_array.unsafe_set_none h.data 0;
    h.count <- h.count - 1;
    if h.count > 0 then begin
      Base.Option_array.swap h.data 0 h.count;
      go_down 0 h
    end

  let min_elt h = h.data.+(0)
  let min_elt_opt h =
    if h.count > 0 then Some h.data.+(0) else None

  let cardinal h = h.count
  let[@inline] is_empty h = h.count = 0
end
