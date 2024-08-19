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

(** Low link.

    Beware: this function does not compute the low links for some nodes
    if they are unreachable from a starting node. If necessary, use [lowlink] instead.

    Return an iterator of entrance and exit events.
    When it yields [`Enter v], [dist.(v)], [from.(v)], and [ord.(v)] are guaranteed to be determined.
    When it yields [`Leave v], [low.(v)] is guaranteed to be determined.

    After running [lowlink ~g ~dist ~from ~ord ~low start],
    - [ord.(v)] is how many nodes are visited before [v].
    - [low.(v)] is the minimum [ord] of nodes reachable from the subtree rooted at [v]
      using at most one back edge.
    - [from.(v)] is the parent of [v] in a DFS tree rooted at [start] if [v] is
      reachable from [start] and is not [start].
  *)
val lowlink_one :
  g:int graph -> dist:int array -> ?from:int array ->
    ord:int array -> low:int array -> int -> inout_event Iter.t

(** Low link.

    In contract to [lowlink_one], this function computes all low links every time.
  *)
val lowlink :
  g:int graph -> dist:int array -> ?from:int array ->
    ord:int array -> low:int array -> inout_event Iter.t

(** Strongly connected components.
    
    This function is intended to be combined with [lowlink]; for example,
    {[
      lowlink ~g ~dist ~ord ~low |> scc ~ord ~low
    ]}
  *)
val scc :
  ord:int array -> low:int array -> inout_event Iter.t -> int list list

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
