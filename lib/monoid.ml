module type S = sig
  type t
  val one : t
  val mul : t -> t -> t
end

module PrefixSumMonoid : S with type t = int * int = struct
  type t = int * int
  let one = (0, -1_000_000_000_000_000_000)
  let mul (s, m) (s', m') = (s + s', max m (s + m'))
end

module IntPairMinMonoid : S with type t = int * int = struct
  type t = int * int
  let one = (max_int, max_int)
  let mul p q = min p q
end

module IntAddMonoid : S with type t = int = struct
  type t = int
  let one = 0
  let mul x y = x + y
end
