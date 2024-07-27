open Math

type t = int
let modulus = 998_244_353

let of_int i =
  let i = i mod modulus in
  if i >= 0 then i else i + modulus
let to_int i = i

let zero = 0
let one = 1

let add i j =
  let k = i + j in
  if k < modulus then k else k - modulus
let sub i j =
  let k = i - j in
  if k >= 0 then k else k + modulus
let mul i j = i * j mod modulus
let inv i =
  assert (i <> 0);
  let (inv_i, _, _) = extgcd i modulus in
  if inv_i >= 0 then inv_i else inv_i + modulus
let div i j =
  assert (j <> 0);
  mul i (inv j)
let[@inline] rec pow_aux a n p =
  if n = 0 then
    p
  else if n mod 2 = 1 then
    pow_aux (mul a a) (n / 2) (mul p a)
  else
    pow_aux (mul a a) (n / 2) p
let pow a n = pow_aux a n 1
let neg i = if i > 0 then modulus - i else 0

let (+%) i j = add i j
let (-%) i j = sub i j
let ( *% ) i j = mul i j
let ( /% ) i j = div i j
let (^%) a n = pow a n
let (~-%) i = neg i
let (~/%) i = inv i
