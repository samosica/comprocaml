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
      let v = Stack.pop stack in
      k v;
      g.(v) |> List.iter (fun w ->
        if dist.(w) = -1 then begin
          dist.(w) <- dist.(v) + 1;
          Stack.push w stack;
          Option.iter (fun from -> from.(w) <- v) from
        end
      );
      dfs_aux k
    end in
  assert (dist.(start) <> -1);
  Stack.push start stack;
  Iter.from_iter dfs_aux

type inout_event = [`Enter of int | `Leave of int]

let dfs_inout ~g ~dist ?from start =
  (*
    Internally, events are represented as (node index) * 2 + (event type)
    (event type 0 for entrances, and 1 for exits) for efficiency.
    Other representations ((polymorphic) variants and tuples) were
    1.2 to 1.35 times slower for 10^7 nodes.
  *)
  let stack = Stack.create() in
  let rec dfs_aux k =
    if not (Stack.is_empty stack) then begin
      let ev = Stack.pop stack in
      let t = ev land 1 in
      let v = ev lsr 1 in
      (match t with
      | 0 ->
        k (`Enter v);
        Stack.push (ev lor 1) stack;
        g.(v) |> List.iter (fun w ->
          if dist.(w) = -1 then begin
            dist.(w) <- dist.(v) + 1;
            Stack.push (w lsl 1) stack;
            Option.iter (fun from -> from.(w) <- v) from
          end
        )
      | _ -> k (`Leave v));
      dfs_aux k
    end in
  assert (dist.(start) <> -1);
  Stack.push (start lsl 1) stack;
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
        );
        dijkstra_aux k
      end
    end in
  start |> Iter.iter (fun v ->
    assert (dist.(v) <> -1);
    IntPairHeap.add (dist.(v), v) queue
  );
  Iter.from_iter dijkstra_aux
