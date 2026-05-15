open Comprocaml

module SegTree = LazySegmentTree.Make (Monoid.IntMaxMonoid) (Monoid.IntAddAction)

let%test "at" =
  let t = SegTree.make 4 in
  for i = 0 to 3 do
    SegTree.set i 0 t
  done;
  for i = 0 to 3 do
    let v = if i mod 2 = 0 then i + 1 else -(i + 1) in
    SegTree.apply_range 0 (i + 1) v t
  done;
  List.init 4 (fun i -> SegTree.at i t) = [-2; -3; -1; -4]
