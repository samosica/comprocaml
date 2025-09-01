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

let dfs ~g ~dist ?from start =
  let stack = Stack.create() in
  let rec dfs_aux k =
    if not (Stack.is_empty stack) then begin
      let v, p = Stack.pop stack in
      if p = -1 || dist.(v) = -1 then begin
        if p <> -1 then begin
          dist.(v) <- dist.(p) + 1;
          Option.iter (fun from -> from.(v) <- p) from
        end;
        k v;
        g.(v) |> List.iter (fun w ->
          if dist.(w) = -1 then Stack.push (w, v) stack
        );
      end;
      dfs_aux k
    end in
  assert (dist.(start) <> -1);
  Stack.push (start, -1) stack;
  Iter.from_iter dfs_aux

type inout_event = [`Enter of int | `Leave of int]

let dfs_inout ~g ~dist ?from start =
  (*
    Internally, events are compactly represented as
      (parent node index) * 2^32 + (node index) * 2 + (event type)
    (event type 0 for entrances, and 1 for exits) for efficiency.
    Other representations ((polymorphic) variants and tuples) were
    1.23 to 1.32 times slower for 10^7 nodes.
  *)
  let stack = Stack.create() in
  let rec dfs_aux k =
    if not (Stack.is_empty stack) then begin
      let ev = Stack.pop stack in
      let t = ev land 1 in
      let v = (ev lsr 1) land 0x7fffffff in
      let p = ev asr 32 in
      (match t with
      | 0 ->
        if p = -1 || dist.(v) = -1 then begin
          if p <> -1 then begin
            dist.(v) <- dist.(p) + 1;
          end;
          Option.iter (fun from -> from.(v) <- p) from;
          k (`Enter v);
          Stack.push (ev lor 1) stack;
          g.(v) |> List.iter (fun w ->
            if dist.(w) = -1 then Stack.push ((v lsl 32) lor (w lsl 1)) stack
          )
        end
      | _ -> k (`Leave v));
      dfs_aux k
    end in
  assert (dist.(start) <> -1);
  Stack.push (((-1) lsl 32) lor (start lsl 1)) stack;
  Iter.from_iter dfs_aux

let tour ~in_ ~out ord =
  let count = ref 0 in
  ord
  |> Iter.iter (fun ev ->
    match ev with
    | `Enter v ->
      in_.(v) <- !count;
      incr count
    | `Leave v ->
      out.(v) <- !count;
      incr count
  )

type lowlink_event = [inout_event | `End_of_component of int list]

let lowlink_one ~g ~dist ?from ~ord ~low ~count start =
  (*
    Internally, events are compactly represented as
      (parent node index) * 2^32 + (node index) * 2 + (event type)
    (event type 0 for entrances, and 1 for exits) for efficiency.
  *)
  let event_stack = Stack.create() in
  let node_stack = Stack.create() in
  let rec pop_until v l =
    if Stack.is_empty node_stack then
      l
    else
      let w = Stack.pop node_stack in
      (*
        v と同じ連結成分に属する w の ord が以降の low の計算に影響を与えないように
        ord に十分大きな値を加える。
        この関数が呼ばれた時点で v から到達できる頂点については low は計算済みである。
        そして、まだ探索していない頂点については v から到達できないので、(本来の)ord.(w)
        の値は参照してはいけない (DFS 木において先祖の ord のみを参照する)。
        こうしないと有向グラフが与えられたときに間違った結果が
        得られることがある。具体例は test/graph.ml の dp_g にある。
      *)
      ord.(w) <- ord.(w) + Array.length g;
      if w = v then
        v :: l
      else
        pop_until v (w :: l) in
  let rec dfs_aux k =
    if not (Stack.is_empty event_stack) then begin
      let ev = Stack.pop event_stack in
      let t = ev land 1 in
      let v = (ev lsr 1) land 0x7fffffff in
      let p = ev asr 32 in
      (match t with
      | 0 ->
        if p = -1 || dist.(v) = -1 then begin
          ord.(v) <- !count;
          low.(v) <- !count;
          incr count;
          if p <> -1 then begin
            dist.(v) <- dist.(p) + 1;
            Option.iter (fun from -> from.(v) <- p) from
          end;
          k (`Enter v);
          (* ord.(v) だけ Array.length g を引いておく *)
          ord.(v) <- ord.(v) - Array.length g;
          Stack.push (ev lor 1) event_stack;
          Stack.push v node_stack;
          g.(v) |> List.iter (fun w ->
            if dist.(w) = -1 then begin
              Stack.push ((v lsl 32) lor (w lsl 1)) event_stack
            end else begin
              low.(v) <- Int.min low.(v) (ord.(w) + Array.length g)
            end
          )
        end
      | _ ->
        if p <> -1 then low.(p) <- Int.min low.(p) low.(v);
        k (`Leave v);
        if low.(v) = ord.(v) + Array.length g then begin
          k (`End_of_component (pop_until v []))
        end
      );
      dfs_aux k
    end in
  assert (dist.(start) <> -1);
  Stack.push (((-1) lsl 32) lor (start lsl 1)) event_stack;
  Iter.from_iter dfs_aux

let lowlink ~g ~dist ?from ~ord ~low =
  assert (Array.for_all ((=) (-1)) dist);
  let count = ref 0 in
  Iter.(0 -- (Array.length g - 1))
  |> Iter.filter (fun v -> dist.(v) = -1)
  |> Iter.flat_map (fun v ->
    dist.(v) <- 0;
    lowlink_one ~g ~dist ?from ~ord ~low ~count v
  )

let scc (seq : lowlink_event Iter.t) =
  let comps = ref [] in
  seq
  |> Iter.iter (fun ev ->
    match ev with
    | `End_of_component comp -> comps := comp :: !comps
    | _ -> ()
  );
  !comps

let bfs ~g ~dist ?from start =
  let queue = Queue.create() in
  let rec bfs_aux k =
    if not (Queue.is_empty queue) then begin
      let v = Queue.pop queue in
      k v;
      g.(v) |> List.iter (fun w ->
        if dist.(w) = -1 then begin
          dist.(w) <- dist.(v) + 1;
          Queue.push w queue;
          Option.iter (fun from -> from.(w) <- v) from
        end
      );
      bfs_aux k
    end in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    Queue.push v queue
  );
  Iter.from_iter bfs_aux

let rec compl_bfs_aux ~g ~dist ~unused ?from queue k =
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
        Option.iter (fun from -> from.(x) <- v) from;
        loop unused' al in
    loop unused g.(v);
    (* List.length next_unused <= List.length g.(v) *)
    compl_bfs_aux ~g ~dist ~unused:(List.rev !next_unused) ?from queue k
  end

let compl_bfs ~g ~dist ?from start =
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
  Iter.from_iter @@ compl_bfs_aux ~g ~dist ~unused ?from queue

module IntPair = struct
  type t = int * int
  let compare (x, y) (x', y') =
    match Int.compare x x' with
    | 0 -> Int.compare y y'
    | c -> c
end
module IntPairHeap = QuadHeap.Make (IntPair)

let dijkstra ~g ~dist ?from start =
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
            IntPairHeap.add (d', e.dest) queue;
            Option.iter (fun from -> from.(e.dest) <- v) from
          end
        )
      end;
      dijkstra_aux k
    end in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    IntPairHeap.add (dist.(v), v) queue
  );
  Iter.from_iter dijkstra_aux
