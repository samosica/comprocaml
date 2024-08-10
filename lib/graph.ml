type 'a graph = 'a list array
type edge = { dest : int; cost : int }

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

let bfs ~g ~dist start =
  let queue = Queue.create() in
  let rec bfs_aux k =
    if not (Queue.is_empty queue) then begin
      let v = Queue.pop queue in
      k v;
      g.(v) |> List.iter (fun w ->
        if dist.(w) = -1 then begin
          dist.(w) <- dist.(v) + 1;
          Queue.push w queue
        end
      );
      bfs_aux k
    end in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    Queue.push v queue
  );
  Iter.from_iter bfs_aux

let rec compl_bfs_aux ~g ~dist ~unused queue k =
  if not (Queue.is_empty queue) then begin
    let v = Queue.pop queue in
    k v;
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
        loop unused' al in
    loop unused g.(v);
    compl_bfs_aux ~g ~dist ~unused:(List.rev !next_unused) queue k
  end

let compl_bfs ~g ~dist start =
  assert (Array.for_all (Base.List.is_sorted ~compare:Int.compare) g);
  let unused =
    Iter.(0 -- (Array.length g - 1))
    |> Iter.filter (fun v -> dist.(v) = -1)
    |> Iter.to_list in
  let queue = Queue.create() in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    Queue.push v queue
  );
  Iter.from_iter @@ compl_bfs_aux ~g ~dist ~unused queue

module IntPair = struct
  type t = int * int
  let compare (x, y) (x', y') =
    match Int.compare x x' with
    | 0 -> Int.compare y y'
    | c -> c
end
module IntPairHeap = QuadHeap.Make (IntPair)

let dijkstra ~g ~dist start =
  (* TODO: adjust capacity *)
  let queue = IntPairHeap.create ~cap:1 in
  let rec dijkstra_aux k =
    if not (IntPairHeap.is_empty queue) then begin
      let d, v = IntPairHeap.min_elt queue in
      IntPairHeap.remove_min queue;
      if d <= dist.(v) then begin
        k v;
        g.(v) |> List.iter (fun e ->
          let d' = d + e.cost in
          if dist.(e.dest) = -1 || d' < dist.(e.dest) then begin
            dist.(e.dest) <- d';
            IntPairHeap.add (d', e.dest) queue
          end
        );
        dijkstra_aux k
      end
    end in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    IntPairHeap.add (dist.(v), v) queue
  );
  Iter.from_iter dijkstra_aux
