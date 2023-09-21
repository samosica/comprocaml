let[@inline] (let>) i f = Iter.flat_map f i
let[@inline] (let>!) i f = Iter.iter f i

(* Note: [guard] can be defined by [Iter.singleton] and [Iter.empty],
 * but the implementation is less efficient when you use flambda
 *)
let[@inline] guard b k = if b then k ()
