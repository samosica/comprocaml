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

let interleave v sp =
  InOrder([Literal (`Enter v)] @ sp @ [Literal (`Leave v)])

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
