type 'a graph = 'a list array
type edge = { dest : int; cost : int }

(** 連結成分ごとに分解する *)
val decomp : int -> int list array -> int list list

(** Depth-first search.

    After running [dfs ~g ~dist ~from start], [from.(v)] is the parent of [v] in a DFS
    tree rooted at [start] if [v] is reachable from [start] and is not [start].
  *)
val dfs : g:int graph -> dist:int array -> ?from:int array -> int -> int Iter.t

type inout_event = [`Enter of int | `Leave of int]

(** Depth-first search with tracking entrance and exit events.

    After running [dfs ~g ~dist ~from start], [from.(v)] is the parent of [v] in a DFS
    tree rooted at [start] if [v] is reachable from [start] and is not [start].
  *)
val dfs_inout : g:int graph -> dist:int array -> ?from:int array -> int -> inout_event Iter.t

(** Euler tour.

    This function is intended to be combined with [dfs_inout]; for example,
    {[
      s |> dfs_inout ~g ~dist |> tour ~in_ ~out
    ]}

    After running [tour ~in_ ~out seq],
    - [`Enter v] is at position [in_.(v)]
    - [`Leave v] is at position [out.(v)]

    [in_] and [out] are useful for processing queries on a tree such as
    lowest common ancestors (LCAs).
  *)
val tour : in_:int array -> out:int array -> inout_event Iter.t -> unit

(** Breadth-first search.

    After running [bfs ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].
  *)
val bfs : g:int graph -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t

(** Breadth-first search on a complement graph.

    [g.(v)] must be sorted in increasing order for each [v].

    After running [compl_bfs ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].

    The time complexity is O(|V| + |E|).
  *)
val compl_bfs : g:int list array -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t

(** Dijkstra's algorithm.

    After running [dijkstra ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].
  *)
val dijkstra : g:edge graph -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t
