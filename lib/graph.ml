(* 連結成分ごとに分解する *)
let decomp n al =
  let cs = ref [] in
  let used = Array.make (n + 1) false in
  let rec loop v c =
    used.(v) <- true;
    c := v :: !c;
    List.iter (fun w -> if not used.(w) then loop w c) al.(v) in
  let () =
    for v = 1 to n do
      if not used.(v) then begin
        let c = ref [] in
        loop v c; cs := !c :: !cs
      end
    done in
  !cs

(* それぞれの頂点の深さを計算する *)
(* i 番目の頂点の深さは depth.(i) に格納される *)
(* 使用方法: compute_depth (-1) (起点) 0 (隣接リスト) depth *)
let rec compute_depth p v d al depth =
  depth.(v) <- d;
  List.iter (fun w ->
    if w <> p then compute_depth v w (d + 1) al depth
  ) al.(v)

(* Euler tour を計算する *)
(* in_.(i) には i 番目の頂点に入ったタイミングが記録される *)
(* out.(i) には i 番目の頂点から出て行ったタイミングが記録される *)
(* in_, out を使うと LCA を計算したり、木上で1点更新、パスクエリが実装できる *)
(* 使用方法: tour (-1) (起点) (ref 0) (隣接リスト) in_ out *)
let rec tour p v c al in_ out =
  in_.(v) <- !c;
  incr c;
  List.iter (fun w ->
    if w <> p then tour v w c al in_ out
  ) al.(v);
  out.(v) <- !c;
  incr c

let rec bfs_aux queue al dist k =
  if not (Queue.is_empty queue) then begin
    let v = Queue.pop queue in
    k v;
    Iter.of_list al.(v)
    |> Iter.filter (fun w -> dist.(w) = -1)
    |> Iter.iter (fun w ->
      dist.(w) <- dist.(v) + 1;
      Queue.add w queue
    );
    bfs_aux queue al dist k
  end

let bfs queue al dist = Iter.from_iter @@ bfs_aux queue al dist

let rec compl_bfs_aux queue unused al dist k =
  if not @@ Queue.is_empty queue then begin
    let v = Queue.pop queue in
    let next_unused = ref [] in
    let rec loop unused al =
      match unused, al with
      | [], _ -> ()
      | x :: _, y :: al' when x > y -> (* x <= y となるまでスキップ *)
        loop unused al'
      | x :: unused', y :: al' when x = y ->
        (* x ∈ al.(v) なので、x はまだ使えない *)
        next_unused := x :: !next_unused;
        loop unused' al'
      | x :: unused', _ ->
        (* x ∉ al.(v) なので、v から x に移動できる *)
        Queue.add x queue;
        dist.(x) <- dist.(v) + 1;
        k x;
        loop unused' al in
    loop unused al.(v);
    compl_bfs_aux queue (List.rev !next_unused) al dist k
  end

(** 補グラフ上の幅優先探索。
  [unused]、[al.(v)]はソートされている必要がある
  *)
let compl_bfs queue unused al dist =
  assert (Base.List.is_sorted ~compare:Int.compare unused);
  assert (Array.for_all (Base.List.is_sorted ~compare:Int.compare) al);
  Iter.from_iter @@ compl_bfs_aux queue unused al dist
