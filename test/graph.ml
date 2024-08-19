open Comprocaml
open Graph

type 'a order_spec =
| Literal of 'a
| InOrder of 'a order_spec list
| Swappable of 'a order_spec * 'a order_spec

let rec valid_orders = function
| Literal l -> [[l]]
| InOrder [] -> [[]]
| InOrder (sp :: spl) ->
  valid_orders sp |> List.concat_map (fun l1 ->
    valid_orders (InOrder spl) |> List.map (fun l2 ->
      l1 @ l2
    )
  )
| Swappable (sp1, sp2) -> 
  valid_orders (InOrder [sp1; sp2]) @ valid_orders (InOrder [sp2; sp1])

let is_valid l sp = List.mem l (valid_orders sp)

let%test "dfs(binary tree)" =
  let n = 8 in
  let g = 
    Array.init n @@ fun i ->
      (if i > 0 then [(i - 1) / 2] else [])
        @ List.filter (fun i -> i < n) [i * 2 + 1; i * 2 + 2] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    0
    |> dfs ~g ~dist ~from
    |> Iter.to_list in
  let sp =
    InOrder([
      Literal(0);
      Swappable(
        InOrder([
          Literal(1);
          Swappable(
            InOrder([
              Literal(3);
              Literal(7);
            ]),
            Literal(4)
          );
        ]),
        InOrder([
          Literal(2);
          Swappable(
            Literal(5),
            Literal(6)
          );
        ])
      );
    ]) in
  Iter.(for_all (fun v -> dist.(v) = Base.Int.floor_log2 (v + 1)) (0 -- (n - 1)))
    && is_valid ord sp
    && from.(0) = -1
    && Iter.(for_all (fun v -> from.(v) = (v - 1) / 2) (1 -- (n - 1)))

let%test "dfs(binary tree): check intermediate state" =
  let n = 8 in
  let g = 
    Array.init n @@ fun i ->
      (if i > 0 then [(i - 1) / 2] else [])
        @ List.filter (fun i -> i < n) [i * 2 + 1; i * 2 + 2] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ok = ref true in
  0
  |> dfs ~g ~dist ~from
  |> Iter.iter (fun v ->
    if dist.(v) <> Base.Int.floor_log2 (v + 1) then ok := false;
    if from.(v) <> if v = 0 then -1 else (v - 1) / 2 then ok := false
  );
  !ok

let%test "dfs(hexagon)" =
  let n = 6 in
  let g = 
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    0
    |> dfs ~g ~dist ~from
    |> Iter.to_list in
  (
    dist = [| 0; 1; 2; 3; 4; 5 |]
    && from = [| -1; 0; 1; 2; 3; 4 |]
    && ord = [0; 1; 2; 3; 4; 5]
  )
    || (
      dist = [| 0; 5; 4; 3; 2; 1 |]
      && from = [| -1; 2; 3; 4; 5; 0 |]
      && ord = [0; 5; 4; 3; 2; 1]
    )

let%test "dfs_inout(binary tree)" =
  let n = 8 in
  let g = 
    Array.init n @@ fun i ->
      (if i > 0 then [(i - 1) / 2] else [])
        @ List.filter (fun i -> i < n) [i * 2 + 1; i * 2 + 2] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    0
    |> dfs_inout ~g ~dist ~from
    |> Iter.to_list in
  let interleave v sp =
    InOrder([Literal (`Enter v)] @ sp @ [Literal (`Leave v)]) in
  let sp =
    interleave 0 [
      Swappable(
        interleave 1 [
          Swappable(
            interleave 3 [
              interleave 7 [];
            ],
            interleave 4 []
          );
        ],
        interleave 2 [
          Swappable(
            interleave 5 [],
            interleave 6 []
          );
        ]
      );
    ] in
  Iter.(for_all (fun v -> dist.(v) = Base.Int.floor_log2 (v + 1)) (0 -- (n - 1)))
    && is_valid ord sp
    && from.(0) = -1
    && Iter.(for_all (fun v -> from.(v) = (v - 1) / 2) (1 -- (n - 1)))

let%test "dfs_inout(binary tree): check intermediate state" =
  let n = 8 in
  let g = 
    Array.init n @@ fun i ->
      (if i > 0 then [(i - 1) / 2] else [])
        @ List.filter (fun i -> i < n) [i * 2 + 1; i * 2 + 2] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ok = ref true in
  0
  |> dfs_inout ~g ~dist ~from
  |> Iter.iter (function
    | `Enter v ->
      if dist.(v) <> Base.Int.floor_log2 (v + 1) then ok := false;
      if from.(v) <> if v = 0 then -1 else (v - 1) / 2 then ok := false
    | _ -> ()
  );
  !ok

let%test "dfs_inout(hexagon)" =
  let n = 6 in
  let g = 
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    0
    |> dfs_inout ~g ~dist ~from
    |> Iter.to_list in
  let interleave v l = [`Enter v] @ l @ [`Leave v] in
  (
    dist = [| 0; 1; 2; 3; 4; 5 |]
    && from = [| -1; 0; 1; 2; 3; 4 |]
    && ord = List.fold_right interleave [0; 1; 2; 3; 4; 5] []
  )
    || (
      dist = [| 0; 5; 4; 3; 2; 1 |]
      && from = [| -1; 2; 3; 4; 5; 0 |]
      && ord = List.fold_right interleave [0; 5; 4; 3; 2; 1] []
    )

let%test "tour(binary tree)" =
  let n = 8 in
  let g = 
    Array.init n @@ fun i ->
      (if i > 0 then [(i - 1) / 2] else [])
        @ List.filter (fun i -> i < n) [i * 2 + 1; i * 2 + 2] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let ord =
    0
    |> dfs_inout ~g ~dist
    |> Iter.to_list in
  let in_ = Array.make n (-1) in
  let out = Array.make n (-1) in
  Iter.of_list ord |> tour ~in_ ~out;
  Base.List.is_sorted ~compare:(fun ev1 ev2 ->
    let get_time = function | `Enter v -> in_.(v) | `Leave v -> out.(v) in
    Int.compare (get_time ev1) (get_time ev2)
  ) ord

let%test "lowlink_one(hexagon)" =
  let n = 6 in
  let g = 
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord = Array.make n (-1) in
  let low = Array.make n (-1) in
  lowlink_one ~g ~dist ~from ~ord ~low 0 |> Iter.iter ignore;
  (
    dist = [| 0; 1; 2; 3; 4; 5 |]
    && from = [| -1; 0; 1; 2; 3; 4 |]
    && ord = [| 0; 1; 2; 3; 4; 5 |]
    && low = [| 0; 0; 0; 0; 0; 0 |]
  )
    || (
      dist = [| 0; 5; 4; 3; 2; 1 |]
      && from = [| -1; 2; 3; 4; 5; 0 |]
      && ord = [| 0; 5; 4; 3; 2; 1 |]
      && low = [| 0; 0; 0; 0; 0; 0 |]
    )

let%test "lowlink_one(hexagon): check intermediate state" =
  let n = 6 in
  let g = 
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord = Array.make n (-1) in
  let low = Array.make n (-1) in
  let dist_ent = Array.make n (-1) in
  let from_ent = Array.make n (-1) in
  let ord_ent = Array.make n (-1) in
  let low_ex = Array.make n (-1) in
  lowlink_one ~g ~dist ~from ~ord ~low 0
  |> Iter.iter (function
    | `Enter v ->
      dist_ent.(v) <- dist.(v);
      from_ent.(v) <- from.(v);
      ord_ent.(v) <- ord.(v)
    | `Leave v ->
      low_ex.(v) <- low.(v)
  );
  (
    dist_ent = [| 0; 1; 2; 3; 4; 5 |]
    && from_ent = [| -1; 0; 1; 2; 3; 4 |]
    && ord_ent = [| 0; 1; 2; 3; 4; 5 |]
    && low_ex = [| 0; 0; 0; 0; 0; 0 |]
  )
    || (
      dist_ent = [| 0; 5; 4; 3; 2; 1 |]
      && from_ent = [| -1; 2; 3; 4; 5; 0 |]
      && ord_ent = [| 0; 5; 4; 3; 2; 1 |]
      && low_ex = [| 0; 0; 0; 0; 0; 0 |]
    )

let%test "lowlink(two independent nodes)" =
  let n = 2 in
  let g = Array.make n [] in
  let dist = Array.make n (-1) in
  let from = Array.make n (-1) in
  let ord = Array.make n (-1) in
  let low = Array.make n (-1) in
  let ord' = lowlink ~g ~dist ~from ~ord ~low |> Iter.to_array in
  from = [| -1; -1 |]
    && ord = [| 0; 0 |]
    && low = [| 0; 0 |]
    && ord' = [| `Enter 0; `Leave 0; `Enter 1; `Leave 1 |]

let%test "bfs(tree)" =
  let n = 7 in
  let g = [|
    [1; 2];
    [0; 3; 4];
    [0; 6];
    [1];
    [1; 5];
    [4];
    [2];
  |] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton 0
    |> bfs ~g ~dist ~from
    |> Iter.to_array in
  dist = [| 0; 1; 1; 2; 2; 3; 2 |]
    && Array.length ord = n
    && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1)))
    && Base.Array.is_sorted ~compare:(fun v w -> Int.compare dist.(v) dist.(w)) ord
    && from = [| -1; 0; 0; 1; 1; 4; 2 |] (* no ambiguity *)

let%test "bfs(tree): check intermediate state" =
  let n = 7 in
  let g = [|
    [1; 2];
    [0; 3; 4];
    [0; 6];
    [1];
    [1; 5];
    [4];
    [2];
  |] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ok = ref true in
  Iter.singleton 0
  |> bfs ~g ~dist ~from
  |> Iter.iter (fun v ->
    if dist.(v) <> [| 0; 1; 1; 2; 2; 3; 2 |].(v) then ok := false;
    if from.(v) <> [| -1; 0; 0; 1; 1; 4; 2 |].(v) then ok := false
  );
  !ok

let%test "bfs(rectangle)" =
  let n = 4 in
  let g =
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let s = 1 in
  let dist = Array.make n (-1) in
  dist.(s) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton s
    |> bfs ~g ~dist ~from
    |> Iter.to_array in
  dist = [| 1; 0; 1; 2 |]
    && Array.length ord = n
    && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1)))
    && Base.Array.is_sorted ~compare:(fun v w -> Int.compare dist.(v) dist.(w)) ord
    && Iter.(for_all (fun v ->
      if from.(v) = -1 then
        v = s
      else
        List.mem v g.(from.(v)) && dist.(v) = dist.(from.(v)) + 1
    ) (0 -- (n - 1)))

let%test "dijkstra(tree)" =
  let n = 7 in
  let g = Array.make n [] in
  [
    (0, 1, 2);
    (0, 2, 1);
    (1, 3, 3);
    (1, 4, 1);
    (2, 6, 3);
    (4, 5, 3);
  ] |> List.iter (fun (u, v, c) ->
    ArrayExt.replace g u (List.cons { dest = v; cost = c });
    ArrayExt.replace g v (List.cons { dest = u; cost = c });
  );
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton 0
    |> dijkstra ~g ~dist ~from
    |> Iter.to_array in
  dist = [| 0; 2; 1; 5; 3; 6; 4 |]
    && ord = [| 0; 2; 1; 4; 6; 3; 5 |] (* no ambiguity *)
    && from = [| -1; 0; 0; 1; 1; 4; 2 |] (* no ambiguity *)

let%test "dijkstra(haste makes waste)" =
  let n = 200 in
  let g = Array.make n [] in
  for v = 1 to n - 2 do
    ArrayExt.replace g 0 (List.cons { dest = v; cost = v });
    ArrayExt.replace g v (List.cons { dest = n - 1; cost = if v < n - 2 then 300 - v else 299 - v });
  done;
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton 0
    |> dijkstra ~g ~dist ~from
    |> Iter.to_array in
  dist.(0) = 0
    && dist.(n - 1) = 299
    && Iter.(for_all (fun v -> dist.(v) = v) (1 -- (n - 2)))
    && ord = Array.init n Fun.id (* no ambiguity *)
    && from.(0) = -1
    && from.(n - 1) = n - 2
    && Iter.(for_all (fun v -> from.(v) = 0) (1 -- (n - 2)))

let%test "dijkstra(haste makes waste): check intermediate state" =
  let n = 200 in
  let g = Array.make n [] in
  for v = 1 to n - 2 do
    ArrayExt.replace g 0 (List.cons { dest = v; cost = v });
    ArrayExt.replace g v (List.cons { dest = n - 1; cost = if v < n - 2 then 300 - v else 299 - v });
  done;
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ok = ref true in
  Iter.singleton 0
  |> dijkstra ~g ~dist ~from
  |> Iter.iter (fun v ->
    if dist.(v) <> (if v = 0 then 0 else if v = n - 1 then 299 else v) then ok := false;
    if from.(v) <> (if v = 0 then -1 else if v = n - 1 then n - 2 else 0) then ok := false
  );
  !ok

let%test "compl_bfs rejects not sorted adjacency list" =
  let g = [|
    [2; 1];
    [0; 2];
    [0; 1];
  |] in
  let dist = [| 0; -1; -1 |] in
  try
    Iter.singleton 0
    |> compl_bfs ~g ~dist
    |> Iter.iter ignore;
    false
  with
    Assert_failure(_, _, _) -> true

let%test "compl_bfs(path)" =
  let n = 4 in
  (* the complement graph of [g] is the path 0 -> 2 -> 3 -> 1 *)
  let g = [|
    [1; 3];
    [0; 2];
    [1];
    [0];
  |] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton 0
    |> compl_bfs ~g ~dist ~from
    |> Iter.to_array in
  dist = [| 0; 3; 1; 2 |]
    && ord = [| 0; 2; 3; 1 |] (* no ambiguity *)
    && from = [| -1; 3; 0; 2 |] (* no ambiguity *)

let%test "compl_bfs(path): check intermediate state" =
  let n = 4 in
  (* the complement graph of [g] is the path 0 -> 2 -> 3 -> 1 *)
  let g = [|
    [1; 3];
    [0; 2];
    [1];
    [0];
  |] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ok = ref true in
  Iter.singleton 0
  |> compl_bfs ~g ~dist ~from
  |> Iter.iter (fun v ->
    if dist.(v) <> [| 0; 3; 1; 2 |].(v) then ok := false;
    if from.(v) <> [| -1; 3; 0; 2 |].(v) then ok := false
  );
  !ok

let%test "compl_bfs(directed)" =
  let n = 4 in
  (*
    the complement graph of [g] is the path 0 -> 1 -> 2 -> 3
    with an edge from 1 to 3
  *)
  let g = [|
    [2; 3];
    [];
    [0];
    [0; 1];
  |] in
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let from = Array.make n (-1) in
  let ord =
    Iter.singleton 0
    |> compl_bfs ~g ~dist ~from
    |> Iter.to_array in
  dist = [| 0; 1; 2; 2 |]
    && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1)))
    && Base.Array.is_sorted ~compare:(fun v w -> Int.compare dist.(v) dist.(w)) ord
    && from = [| -1; 0; 1; 1 |] (* no ambiguity *)

let%test "compl_bfs(complete graph)" =
  let n = 4 in
  (* the complement graph of [g] has no edge *)
  let g =
    Array.init n @@ fun i ->
      List.init n Fun.id |> List.filter ((<>) i) in
  let dist = [| 0; -1; -1; -1 |] in
  let ord =
    Iter.singleton 0
    |> compl_bfs ~g ~dist
    |> Iter.to_array in
  dist = [| 0; -1; -1; -1 |]
    && ord = [| 0 |] (* no ambiguity *)
