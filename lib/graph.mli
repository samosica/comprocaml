type 'a graph = 'a list array
type edge = { dest : int; cost : int }

(** 連結成分ごとに分解する *)
val decomp : int -> int list array -> int list list

(** Euler tour を計算する。
    in_.(i) には i 番目の頂点に入ったタイミングが記録される。
    out.(i) には i 番目の頂点から出て行ったタイミングが記録される。
    in_, out を使うと LCA を計算したり、木上で1点更新、パスクエリが実装できる。
    使用方法: tour (-1) (起点) (ref 0) (隣接リスト) in_ out
  *)
val tour : int -> int -> int ref -> int list array -> int array -> int array -> unit

val bfs : g:int graph -> dist:int array -> int Iter.t -> int Iter.t

(** Breadth-first search on a complement graph.

    [g.(v)] must be sorted in increasing order for each [v].
  *)
val compl_bfs : g:int list array -> dist:int array -> int Iter.t -> int Iter.t

val dijkstra : g:edge graph -> dist:int array -> int Iter.t -> int Iter.t
