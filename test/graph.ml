open Comprocaml

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
    |> Graph.bfs ~g ~dist
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
    |> Graph.bfs ~g ~dist
    |> Iter.to_array in
  let ok1 = dist = [| 1; 0; 1; 2 |] in
  let ok2 =
    Array.length ord = n
      && Iter.(for_all (fun i -> Array.mem i ord) (0 -- (n - 1))) in
  let ok3 = valid_order ~n ~dist ord in
  ok1 && ok2 && ok3
