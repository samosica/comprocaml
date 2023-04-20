let extgcd a b =
  let rec loop a b s t s' t'  =
    if b = 0 then
      (s, t, a)
    else
      loop b (a mod b) s' t' (s - (a / b) * s') (t - (a / b) * t') in
  loop a b 1 0 0 1

module ModInt998244353: sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
  val (/%) : t -> t -> t
  val (^%) : t -> int -> t
end = struct
  type t = int
  let mo = 998_244_353
  let of_int i =
    let i = i mod mo in
    if i >= 0 then i else i + mo
  let to_int i = i
  let zero = 0
  let one = 1
  let add i j =
    let k = i + j in
    if k < mo then k else k - mo
  let sub i j =
    let k = i - j in
    if k >= 0 then k else k + mo
  let mul i j = i * j mod mo
  let div i j =
    assert (j <> 0);
    let (inv_j, _, _) = extgcd j mo in
    let inv_j =
      if inv_j >= 0 then
        inv_j
      else
        inv_j + mo in
    mul i inv_j
  let (+%) i j = add i j
  let (-%) i j = sub i j
  let ( *% ) i j = mul i j
  let ( /% ) i j = div i j
  let rec pow_aux a n p =
    if n = 0 then
      p
    else if n mod 2 = 1 then
      pow_aux (a *% a) (n / 2) (p *% a)
    else
      pow_aux (a *% a) (n / 2) p
  let[@inline] (^%) a n = pow_aux a n 1
end

module ModInt (M : sig val mo : int end) : sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
end = struct
  type t = int
  let of_int i =
    let i = i mod M.mo in
    if i >= 0 then i else i + M.mo
  let to_int i = i
  let zero = 0
  let one = if M.mo <> 1 then 1 else 0
  let add i j =
    let k = i + j in
    if k < M.mo then k else k - M.mo
  let sub i j =
    let k = i - j in
    if k >= 0 then k else k + M.mo
  let mul i j = i * j mod M.mo
  let (+%) i j = add i j
  let (-%) i j = sub i j
  let ( *% ) i j = mul i j
end
