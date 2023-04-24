let (let>) i f = Iter.flat_map f i
let (let>!) i f = Iter.iter f i

let guard = function
| false -> Iter.empty
| true -> Iter.singleton ()