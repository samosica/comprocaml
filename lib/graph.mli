(*
  References:
  - Strongly connected components
    - [Tarjan's strongly connected components algorithm - Wikipedia](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm). Retrieved 2024-08-20.
    - [ac-library/atcoder/internal_scc.hpp at fe9b6fca9ab4e1be946ea23a4e6a2a751cf4aaa2 · atcoder/ac-library](https://github.com/atcoder/ac-library/blob/fe9b6fca9ab4e1be946ea23a4e6a2a751cf4aaa2/atcoder/internal_scc.hpp). Retrieved 2024-08-20.
*)

type 'a graph = 'a list array
type edge = { dest : int; cost : int }

(** 連結成分ごとに分解する *)
val decomp : int -> int list array -> int list list

(** Depth-first search.

    Return an iterator of nodes which are sorted in a DFS order.
    When it yields node [v], [dist.(v)] and [from.(v)] are guaranteed to be determined.

    After running [dfs ~g ~dist ~from start], [from.(v)] is the parent of [v] in a DFS
    tree rooted at [start] if [v] is reachable from [start] and is not [start].
  *)
val dfs : g:int graph -> dist:int array -> ?from:int array -> int -> int Iter.t

type inout_event = [`Enter of int | `Leave of int]

(** Depth-first search with tracking entrance and exit events.

    Return an iterator of events.
    When it yields [`Enter v], [dist.(v)] and [from.(v)] are guaranteed to be determined.

    After running [dfs_inout ~g ~dist ~from start], [from.(v)] is the parent of [v] in a DFS
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

type lowlink_event = [inout_event | `End_of_component of int list]

(** Low link for all nodes.

    Return an iterator of entrance, exit, and end-of-component events.
    When it yields [`Enter v], [dist.(v)], [from.(v)] are guaranteed to be determined.
    In addition, [ord.(v)] becomes the right value, but after that, it will not be
    guaranteed until the end of the execution.
    When it yields [`Leave v], [low.(v)] is guaranteed to be determined.
    When it yields [`End_of_component c], [c] is one of strongly connected components.
    For all strongly connected components [c1] and [c2], if [c2] is reachable from
    [c1], then [`End_of_component c2] will occur before [`End_of_component c1].

    After running [lowlink ~g ~dist ~from ~ord ~low],
    - [ord.(v)] is how many nodes are visited before [v].
    - [low.(v)] is the minimum [ord] of nodes reachable from [v] using edges in
      a DFS tree and at most one back edge.
    - [from.(v)] is the parent of [v] in a DFS tree if [v] is not the root of
      the DFS tree; otherwise -1.
  *)
val lowlink :
  g:int graph -> dist:int array -> ?from:int array ->
    ord:int array -> low:int array -> lowlink_event Iter.t

(** Strongly connected components.

    Return a list of strongly connected components.
    For all strongly connected components [c1] and [c2], if [c2] is reachable from
    [c1], then [c1] occurs before [c2] in the list.
    N.B. this order is the reverse of the order in [lowlink].

    This function is intended to be combined with [lowlink]; for example,
    {[
      lowlink ~g ~dist ~ord ~low |> scc
    ]}
  *)
val scc : lowlink_event Iter.t -> int list list

(** Breadth-first search.

    Return an iterator of nodes which are sorted in a BFS order.
    When it yields node [v], [dist.(v)] and [from.(v)] are guaranteed to be determined.

    After running [bfs ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].
  *)
val bfs : g:int graph -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t

(** Breadth-first search on a complement graph.

    [g.(v)] must be sorted in increasing order for each [v].

    Return an iterator of nodes which are sorted in a BFS order.
    When it yields node [v], [dist.(v)] and [from.(v)] are guaranteed to be determined.

    After running [compl_bfs ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].

    The time complexity is O(|V| + |E|).
  *)
val compl_bfs : g:int list array -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t

(** Dijkstra's algorithm.

    Return an iterator of nodes which are sorted by their distances from starting nodes.
    When it yields node [v], [dist.(v)] and [from.(v)] are guaranteed to be determined.

    After running [dijkstra ~g ~dist ~from start], [from.(v)] is the last node but one
    in a shortest path from [start] to [v] if [v] is reachable from [start] and
    is not included in [start].
  *)
val dijkstra : g:edge graph -> dist:int array -> ?from:int array -> int Iter.t -> int Iter.t
