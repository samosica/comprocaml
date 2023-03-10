module ModInt998244353: sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val one : t
  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
end = struct
  type t = int
  let mo = 998_244_353
  let of_int i =
    let i = i mod mo in
    if i >= 0 then i else i + mo
  let to_int i = i
  let zero = 0
  let one = 1
  let (+%) i j =
    let k = i + j in
    if k < mo then k else k - mo
  let (-%) i j =
    let k = i - j in
    if k >= 0 then k else k + mo
  let ( *% ) i j = i * j mod mo
end