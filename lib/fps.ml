(** Formal Power Series *)

(** Extended Euclidean algorithm.
    Given a pair (a, b) of integers, returns a triple (s, t, g) of integers
    such that g is the GCD of a and b, and sa + tb = g.
*)
let extgcd a b =
  let rec loop a b s t s' t'  =
    if b = 0 then
      (s, t, a)
    else
      loop b (a mod b) s' t' (s - (a / b) * s') (t - (a / b) * t') in
  loop a b 1 0 0 1

let ceil_log2 n =
  let rec loop log pow = if n <= pow then log else loop (log + 1) (pow * 2) in
  loop 0 1

(** 998244353 - 1 = 2^23 * 7 * 17 *)
module Mod998244353 = struct
  (** = 998,244,353 *)
  let modulus = 998_244_353

  (** a primitive root modulo 998,244,353 *)
  let prim_root = 3

  (* integer arithmetic *)
  (* these functions assume that 0 <= their arguments < modulo *)
  let[@inline] (~-%) i = if i > 0 then modulus - i else 0
  let[@inline] (~/%) i =
    assert (i <> 0);
    let (inv_i, _, _) = extgcd i modulus in
    if inv_i >= 0 then inv_i else inv_i + modulus
  let[@inline] (+%) i j = let k = i + j in if k < modulus then k else k - modulus
  let[@inline] (-%) i j = let k = i - j in if k >= 0 then k else k + modulus
  let[@inline] ( *% ) i j = i * j mod modulus
  let rec pow_aux a n p =
    if n = 0 then
      p
    else if n mod 2 = 1 then
      pow_aux (a *% a) (n / 2) (p *% a)
    else
      pow_aux (a *% a) (n / 2) p
  let[@inline] ( ^% ) a n = pow_aux a n 1

  (** Returns an integer such that clamp i ≡ i (mod 998,244,353) and
      0 <= clamp i < 998,244,353.
    *)
  let[@inline] clamp i =
    let i = i mod modulus in
    if i >= 0 then i else i + modulus
end

module Convolution : sig
  (** Discrete Fourier transform over F_998244353[x].
      Constraint: 0 <= l <= 23 and (the size of a) <= 2^l if you write [fourier998244353 l a].
      This function uses the Cooley-Tukey algorithm.
    *)
  val fourier998244353 : int -> int array -> int array

  (** Inverse discrete Fourier transform over F_998244353[x].
      Constraint: 0 <= l <= 23 and (the size of a) <= 2^l if you write [inv_fourier998244353 l a].
      This function uses the Cooley-Tukey algorithm.
    *)  
  val inv_fourier998244353 : int -> int array -> int array

  (** Convolution modulo 998,244,353.
      Constraint: (the total size of input sequences) - 1 <= 2^23
    *)
  val convolution998244353 : int array -> int array -> int array  
end = struct
  open Mod998244353

  (** Returns a bit-reversal permutation of size 2^l.
      The i-th element (starting with 0) is b(0)b(1)...b(l-1) in binary where
      b(l-1)...b(1)b(0) is the binary representation of i.
    *)
  let bit_reversal l =
    let r = Array.make (1 lsl l) 0 in
    let () =
      for i = 0 to l - 1 do
        let n = 1 lsl i in
        for j = 0 to n - 1 do
          r.(j + n) <- r.(j) + 1 lsl (l - 1 - i)
        done
      done in
    r

  let fourier998244353 l a =
    let n = 1 lsl l in
    let () = assert (0 <= l && l <= 23 && Array.length a <= n) in
    let a' = Array.make n 0 in
    let r = bit_reversal l in
    let () =
      for i = 0 to n - 1 do
        if r.(i) < Array.length a then
          a'.(i) <- clamp a.(r.(i))
      done in
    let () =
      for i = 0 to l - 1 do
        let w = prim_root ^% ((modulus - 1) lsr (i + 1)) in
        for j = 0 to n lsr (i + 1) - 1 do
          let pow_w = ref 1 in
          for k = 0 to 1 lsl i - 1 do
            let x = j lsl (i + 1) + k in
            let y = x + 1 lsl i in
            let u = a'.(x) in
            let v = a'.(y) *% !pow_w in
            a'.(x) <- u +% v;
            a'.(y) <- u -% v;
            pow_w := !pow_w *% w
          done
        done
      done in
    a'

  let inv_fourier998244353 l a =
    let n = 1 lsl l in
    let () = assert (0 <= l && l <= 23 && Array.length a <= n) in
    let a' = Array.make n 0 in
    let r = bit_reversal l in
    let () =
      for i = 0 to n - 1 do
        if r.(i) < Array.length a then
          a'.(i) <- clamp a.(r.(i))
      done in
    let () =
      for i = 0 to l - 1 do
        let w = ~/% (prim_root ^% ((modulus - 1) lsr (i + 1))) in
        for j = 0 to n lsr (i + 1) - 1 do
          let pow_w = ref 1 in
          for k = 0 to 1 lsl i - 1 do
            let x = j lsl (i + 1) + k in
            let y = x + 1 lsl i in
            let u = a'.(x) in
            let v = a'.(y) *% !pow_w in
            a'.(x) <- u +% v;
            a'.(y) <- u -% v;
            pow_w := !pow_w *% w
          done
        done
      done in
    let inv_n = ~/% n in
    let () =
      for i = 0 to Array.length a' - 1 do
        a'.(i) <- a'.(i) *% inv_n
      done in
    a'

  let convolution998244353 a b =
    if Array.length a = 0 || Array.length b = 0 then
      [| |]
    else
      let n = Array.length a + Array.length b - 1 in
      let l = ceil_log2 n in
      let () = assert (l <= 23) in
      let a' = fourier998244353 l a in
      let b' = fourier998244353 l b in
      let () =
        for i = 0 to Array.length a' - 1 do
          a'.(i) <- a'.(i) *% b'.(i)
        done in
      inv_fourier998244353 l a'
end

module Poly998244353 : sig
  type t
  val of_array : int array -> t
  val to_array : t -> int array
  val zero : t
  val one : t
  val x : t
  
  (** Constant term *)
  val (~$) : int -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t

  (** Additive inverse *)
  val neg : t -> t

  (** Multiplicative inverse. The degree of the output polynomial is [degree].
      Constraint: 0 <= [d] < 2^23 and the constant term of [a] is not 0
      if you write [inv ~degree:d a]
    *)
  val inv : degree:int -> t -> t
  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  val ( *: ) : t -> t -> t

  (** Additive inverse *)
  val (~-:) : t -> t
end = struct
  open Mod998244353

  type t = int array

  let of_array a = Array.map clamp a
  let to_array a = a

  let zero = [| |]
  let one = [| 1 |]
  let x = [| 0; 1 |]
  let (~$) i = [| clamp i |]

  let add a b =
    let n = Array.length a in
    let m = Array.length b in
    Array.init (max n m) @@ fun i ->
      let c = if i < n then a.(i) else 0 in
      let c = c +% (if i < m then b.(i) else 0) in
      c

  let sub a b =
    let n = Array.length a in
    let m = Array.length b in
    Array.init (max n m) @@ fun i ->
      let c = if i < n then a.(i) else 0 in
      let c = c -% (if i < m then b.(i) else 0) in
      c

  let mul a b = Convolution.convolution998244353 a b

  let neg a = Array.map (~-%) a

  let (+:) a b = add a b
  let (-:) a b = sub a b
  let ( *: ) a b = mul a b
  let (~-:) a = neg a

  let inv ~degree a =
    (* Invariant: inv_a ≡ 1 / a (mod x^{2^l}) *)
    let rec loop l inv_a =
      if Array.length inv_a >= degree + 1 then
        inv_a
      else
        let a' = Array.init (1 lsl (l + 1)) @@ fun i ->
          if i < Array.length a then
            a.(i)
          else
            0 in
        (* deg(a' * inv_a * inv_a) <= deg(a') + 2 deg(inv_a) <= 2^{l+1} + 2*2^l = 2^{l+2} *)
        let a' = Convolution.fourier998244353 (l + 2) a' in
        let inv_a' = Convolution.fourier998244353 (l + 2) inv_a in
        let () =
          for i = 0 to Array.length a' - 1 do
            a'.(i) <- a'.(i) *% inv_a'.(i) *% inv_a'.(i)
          done in
        let b = Convolution.inv_fourier998244353 (l + 2) a' in
        let b = Array.init (1 lsl (l + 1)) (Array.get b) in
        let () =
          for i = 0 to Array.length b - 1 do
            if i < Array.length inv_a then
              b.(i) <- inv_a.(i)
            else
              b.(i) <- ~-% (b.(i))
          done in
        loop (l + 1) b in
    let inv_a = loop 0 [| ~/% (a.(0)) |] in
    Array.init (degree + 1) (Array.get inv_a)
end