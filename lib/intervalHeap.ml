module type S = sig
  type elt
  type t

  val create : cap:int -> t
  val add : elt -> t -> unit
  val remove_min : t -> unit
  val remove_max : t -> unit

  val min_elt : t -> elt
  val min_elt_opt : t -> elt option

  val max_elt : t -> elt
  val max_elt_opt : t -> elt option

  val cardinal : t -> int
  val is_empty : t -> bool
end

module Make (M : Set.OrderedType) : S with type elt = M.t = struct
  (*
    Internal structure:
    - [data] represents a tree of intervals.
    - The [i]-th (0-based) interval is the one from the [2i]-th element (0-based) of
      [data] to the [2i+1]-th element. But for the last interval, the [2i+1]-th
      element may not exist; in this case, it is the singleton set of the [2i]-th
      element.
    - The [i]-th interval has the [2i+1]-th and [2i+2]-th intervals as its children.
    - If an interval [l1, r1] is the parent of another one [l2, r2], the former
      includes the latter, namely l1 <= l2 <= r2 <= r1 (the invariant of interval heaps).
  *)

  type elt = M.t
  type t = {
    mutable data : elt Base.Option_array.t;
    mutable count : int;
  }

  let[@inline] ( .+() ) a i = Base.Option_array.unsafe_get_some_exn a i
  let[@inline] ( .+()<- ) a i v = Base.Option_array.unsafe_set_some a i v

  let extend a =
    let l = Base.Option_array.length a in
    Base.Option_array.init (2 * l + 2) ~f:(fun i ->
      if i < l then Base.Option_array.unsafe_get a i else None
    )
  let extend h = h.data <- extend h.data
  
  let[@inline] is_full h = h.count = Base.Option_array.length h.data

  let create ~cap =
    let h = { data = Base.Option_array.create ~len:2; count = 0 } in
    let rec loop () =
      if Base.Option_array.length h.data < cap then begin
        extend h; loop ()
      end in
    loop(); h

  (*
    Notes:
    - [i] and [p] point at the lower endpoints of intervals.
      They are therefore even.
    - [go_up_lower] (resp. [go_up_upper]) makes the invariant hold for
      the lower (resp. upper) endpoint of the [p/2]-th interval.
  *)
  let rec go_up_lower i h =
    (* -2 = 0b11...110 *)
    let p = ((i - 2) / 2) land (-2) in
    if i >= 2 && M.compare h.data.+(i) h.data.+(p) < 0 then begin
      Base.Option_array.swap h.data i p;
      go_up_lower p h
    end

  let rec go_up_upper i h =
    (* -2 = 0b11...110 *)
    let p = ((i - 2) / 2) land (-2) in
    if i >= 2 && M.compare h.data.+(i lor 1) h.data.+(p lor 1) > 0 then begin
      Base.Option_array.swap h.data (i lor 1) (p lor 1);
      go_up_upper p h
    end

  let add v h =
    if is_full h then extend h;
    h.data.+(h.count) <- v;
    if h.count land 1 = 0 then begin
      (* -2 = 0b11...110 *)
      let p = ((h.count - 2) / 2) land (-2) in
      if h.count >= 2 then begin
        if M.compare h.data.+(h.count) h.data.+(p) < 0 then begin
          Base.Option_array.swap h.data h.count p;
          go_up_lower p h
        end else if M.compare h.data.+(p lor 1) h.data.+(h.count) < 0 then begin
          Base.Option_array.swap h.data h.count (p lor 1);
          go_up_upper p h
        end
      end
    end else begin
      if M.compare h.data.+(h.count - 1) h.data.+(h.count) <= 0 then begin
        go_up_upper (h.count - 1) h
      end else begin
        Base.Option_array.swap h.data (h.count - 1) h.count;
        go_up_lower (h.count - 1) h
      end
    end;
    h.count <- h.count + 1

  (*
    Notes:
    - [i], [c1], and [min_c] point at the lower endpoints of intervals.
      They are therefore even.
    - [go_down_lower] (resp. [go_down_upper]) makes the invariant hold for
      the lower (resp. upper) endpoint of the [i/2]-th interval.
  *)      
  let rec go_down_lower i h =
    let c1 = i * 2 + 2 in
    let min_c =
      if c1 < h.count && M.compare h.data.+(c1) h.data.+(i) < 0 then
        c1
      else
        i in
    let min_c =
      if c1 + 2 < h.count && M.compare h.data.+(c1 + 2) h.data.+(min_c) < 0 then
        c1 + 2
      else
        min_c in
    if min_c <> i then begin
      Base.Option_array.swap h.data min_c i;
      if min_c lor 1 < h.count && M.compare h.data.+(min_c) h.data.+(min_c lor 1) > 0 then begin
        Base.Option_array.swap h.data min_c (min_c lor 1);
      end;
      go_down_lower min_c h
    end
  
  let rec go_down_upper i h =
    let c1 = i * 2 + 2 in
    let max_c =
      if c1 + 1 < h.count && M.compare h.data.+(c1 + 1) h.data.+(i + 1) > 0 then
        c1
      else
        i in
    let max_c =
      if c1 + 3 < h.count && M.compare h.data.+(c1 + 3) h.data.+(max_c + 1) > 0 then
        c1 + 2
      else
        max_c in
    if max_c <> i then begin
      Base.Option_array.swap h.data (max_c lor 1) (i lor 1);
      if M.compare h.data.+(max_c) h.data.+(max_c lor 1) > 0 then begin
        Base.Option_array.swap h.data (max_c) (max_c lor 1);
      end;
      go_down_upper max_c h
    end

  let cardinal h = h.count
  let[@inline] is_empty h = cardinal h = 0

  let remove_min h =
    Base.Option_array.unsafe_set_none h.data 0;
    h.count <- h.count - 1;
    if h.count >= 1 then begin
      Base.Option_array.swap h.data 0 h.count;
      go_down_lower 0 h
    end

  let remove_max h =
    if cardinal h = 1 then begin
      Base.Option_array.unsafe_set_none h.data 0;
      h.count <- h.count - 1
    end else begin
      Base.Option_array.unsafe_set_none h.data 1;
      h.count <- h.count - 1;
      if h.count >= 2 then begin
        Base.Option_array.swap h.data 1 h.count;
        go_down_upper 0 h
      end
    end

  let min_elt h = h.data.+(0)
  let min_elt_opt h = if cardinal h > 0 then Some (min_elt h) else None

  let max_elt h = if cardinal h >= 2 then h.data.+(1) else h.data.+(0)
  let max_elt_opt h = if cardinal h > 0 then Some (max_elt h) else None
end
