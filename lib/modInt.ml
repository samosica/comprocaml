open Math

module type S = sig
  type t

  val of_int : int -> t
  val to_int : t -> int

  val zero : t
  val one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val inv : t -> t  

  val (+%) : t -> t -> t
  val (-%) : t -> t -> t
  val ( *% ) : t -> t -> t
  val (/%) : t -> t -> t
  val (~/%) : t -> t
  val (^%) : t -> int -> t
end

module Make (M : sig val mo : int end) : S = struct
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
  let inv i =
    let (inv_i, _, g) = extgcd i M.mo in
    assert (g = 1);
    if inv_i >= 0 then inv_i else inv_i + M.mo
  let div i j = mul i (inv j)

  let (+%) i j = add i j
  let (-%) i j = sub i j
  let ( *% ) i j = mul i j
  let ( /% ) i j = div i j
  let (~/%) i = inv i
  let rec pow_aux a n p =
    if n = 0 then
      p
    else if n mod 2 = 1 then
      pow_aux (a *% a) (n / 2) (p *% a)
    else
      pow_aux (a *% a) (n / 2) p
  let[@inline] (^%) a n = pow_aux a n 1
end
