open Comprocaml
open Graph

let (let>) i f = Iter.flat_map f i

(** Return how many nodes were visited before each node. *)
let inv ~n ord =
  let res = Array.make n 0 in
  ord |> Array.iteri (fun i v ->
    res.(v) <- i
  );
  res

let imply b c = not b || c
let iff b c = imply b c && imply c b

let valid_order ~n ~dist ord =
  let inv_ord = inv ~n ord in
  Iter.for_all Fun.id @@
    let> i = Iter.(0 -- (n - 1)) in
    let> j = Iter.(0 -- (n - 1)) in
    Iter.return (
      dist.(i) = -1
        || dist.(j) = -1
        || dist.(i) = dist.(j)
        (*
          node [i] occurs earlier than node [j] in a (valid) BFS order
          <=> node [i] is closer to starting nodes than node [j]
        *)
        || iff (inv_ord.(i) < inv_ord.(j)) (dist.(i) < dist.(j))
  )

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
  let ord =
    Iter.singleton 0
    |> bfs ~g ~dist
    |> Iter.to_array in
  let ok1 = dist = [| 0; 1; 1; 2; 2; 3; 2 |] in
  let ok2 =
    Array.length ord = n
      && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1))) in
  let ok3 = valid_order ~n ~dist ord in
  ok1 && ok2 && ok3

let%test "bfs(rectangle)" =
  let n = 4 in
  let g =
    Array.init n @@ fun i ->
      [(i + 1) mod n; (i + n - 1) mod n] in
  let dist = Array.make n (-1) in
  dist.(1) <- 0;
  let ord =
    Iter.singleton 1
    |> bfs ~g ~dist
    |> Iter.to_array in
  let ok1 = dist = [| 1; 0; 1; 2 |] in
  let ok2 =
    Array.length ord = n
      && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1))) in
  let ok3 = valid_order ~n ~dist ord in
  ok1 && ok2 && ok3

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
  let ord =
    Iter.singleton 0
    |> dijkstra ~g ~dist
    |> Iter.to_array in
  let ok1 = dist = [| 0; 2; 1; 5; 3; 6; 4 |] in
  (* no ambiguity *)
  let ok2 = ord = [| 0; 2; 1; 4; 6; 3; 5 |] in
  ok1 && ok2

let%test "dijkstra(haste makes waste)" =
  let n = 200 in
  let g = Array.make n [] in
  for v = 1 to n - 2 do
    ArrayExt.replace g 0 (List.cons { dest = v; cost = v });
    ArrayExt.replace g v (List.cons { dest = n - 1; cost = if v < n - 2 then 300 - v else 299 - v });
  done;
  let dist = Array.make n (-1) in
  dist.(0) <- 0;
  let ord =
    Iter.singleton 0
    |> dijkstra ~g ~dist
    |> Iter.to_array in
  let ok1 =
    dist.(0) = 0
      && dist.(n - 1) = 299
      && Iter.(for_all (fun v -> dist.(v) = v) (1 -- (n - 2))) in
  (* no ambiguity *)
  let ok2 = ord = Array.init n Fun.id in
  ok1 && ok2

