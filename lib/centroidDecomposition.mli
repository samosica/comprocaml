type 'a indexed_tree

val root_index : 'a indexed_tree -> int
val root_value : 'a indexed_tree -> 'a
val subtrees : 'a indexed_tree -> 'a indexed_tree list

(** 隣接リストから indexed_tree を構築する関数。
    al はグラフの隣接リストで、al.(i) は i 番目の頂点と隣接する頂点の番号のリスト
  *)
val dfs_tree : int list array -> unit indexed_tree

(** 各頂点にそれを根とする部分木のサイズを格納した木を返す関数 *)
val size_tree : 'a indexed_tree -> int indexed_tree

(** 重心分解を行う関数 *)
val centroid_decomp : int indexed_tree -> unit indexed_tree
