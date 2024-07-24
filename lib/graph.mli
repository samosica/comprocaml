(** 連結成分ごとに分解する *)
val decomp : int -> int list array -> int list list

(** それぞれの頂点の深さを計算する。
    i 番目の頂点の深さは depth.(i) に格納される。
    使用方法: compute_depth (-1) (起点) 0 (隣接リスト) depth
  *)
val compute_depth : int -> int -> int -> int list array -> int array -> unit

(** Euler tour を計算する。
    in_.(i) には i 番目の頂点に入ったタイミングが記録される。
    out.(i) には i 番目の頂点から出て行ったタイミングが記録される。
    in_, out を使うと LCA を計算したり、木上で1点更新、パスクエリが実装できる。
    使用方法: tour (-1) (起点) (ref 0) (隣接リスト) in_ out
  *)
val tour : int -> int -> int ref -> int list array -> int array -> int array -> unit

val bfs : int Queue.t -> int list array -> int array -> int Iter.t

(** 補グラフ上の幅優先探索。
  [unused]、[al.(v)]はソートされている必要がある
  *)
val compl_bfs : int Queue.t -> int list -> int list array -> int array -> int Iter.t
