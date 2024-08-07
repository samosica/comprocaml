module Range = struct
  type t = int * int
  let compare (l, r) (l', r') =
    match Int.compare l l' with
    | 0 -> Int.compare r r'
    | c -> c
end

module RangeSet = Set.Make (Range)
include RangeSet

exception Not_disjoint

let find_int_opt x s =
  match RangeSet.find_first_opt (fun (_, r) -> x <= r) s with
  | (Some (l, _) as o) when l <= x -> o
  | _ -> None
let find_int x s = Option.get (find_int_opt x s) 

let find_int_geq_opt x s =
  RangeSet.find_first_opt (fun (_, r) -> x <= r) s
let find_int_geq x s = Option.get (find_int_geq_opt x s)

let find_int_gt_opt x s =
  RangeSet.find_first_opt (fun (l, _) -> x < l) s
let find_int_gt x s = Option.get (find_int_gt_opt x s)

let rec find_ints_aux (left, right) s k =
  if left <= right then begin
    match find_int_geq_opt left s with
    | Some ((l, r) as range) when l <= right ->
      k range;
      if r < Int.max_int then find_ints_aux (r + 1, right) s k
    | _ -> ()
  end
let find_ints r s = Iter.from_iter (find_ints_aux r s)

let mem_int x s = Option.is_some (find_int_opt x s)

let unsafe_add r s = RangeSet.add r s
let add ((left, right) as range) s =
  match find_int_geq_opt left s with
  | Some (l, _) when l <= right -> raise Not_disjoint
  | _ -> RangeSet.add range s
