(* 番号づけられた頂点を持つ木 *)
(* T(i, v, ts) は根の番号が i、格納している値が v で、その下にある部分木のリストが ts である木を表す *)
type 'a indexed_tree = T of int * 'a * 'a indexed_tree list

let root_index (T(i, _, _)) = i
let root_value (T(_, v, _)) = v
let subtrees (T(_, _, ts)) = ts

(* 隣接リストから indexed_tree を構築する関数 *)
(* al はグラフの隣接リストで、al.(i) は i 番目の頂点と隣接する頂点の番号のリスト *)
let dfs_tree al =
  let rec dfs p i =
    T(i, (), List.filter_map (fun j -> if j = p then None else Some (dfs i j)) al.(i)) in
  dfs (-1) 0

(* 各頂点にそれを根とする部分木のサイズを格納した木を返す関数 *)
let rec size_tree t =
  let ts = List.map size_tree (subtrees t) in
  let count = List.fold_left (fun c t -> c + root_value t) 1 ts in
  T(root_index t, count, ts)

(* 重心分解を行う関数 *)
let centroid_decomp t =
  (*  t: いま注目している木 *)
  (* tt: t の上側にあった木のリスト *)
  (*  n: t と tt を合わせた木のサイズ *)
  (* Note: tt のサイズは常に 1 以下になります。そのため、list の代わりに option を使ってもいいですが、 *)
  (*       list の方がきれいに書けます。この小技は時々見ます *)
  let rec decomp t tt n =
    (* N.B. bigger のサイズは常に 1 以下 *)
    let (smaller, bigger) = List.partition (fun t -> root_value t * 2 <= n) (subtrees t) in
    match bigger with
    | t' :: _ ->
      let ts' = tt @ smaller in
      (* t' とそれ以外に分ける *)
      decomp t' [T(root_index t, n - root_value t', ts')] n
    | [] ->
      T(root_index t, (), List.map (fun t -> decomp t [] (root_value t)) (tt @ subtrees t)) in
  decomp t [] (root_value t)
