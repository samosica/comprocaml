module type S = sig
  type t
  val one : t
  val mul : t -> t -> t
end

module type Action = sig
  type t

  module M : S

  val act : t -> M.t -> t
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

module IntMaxMonoid : S with type t = int = struct
  type t = int
  let one = min_int
  let mul x y = Int.max x y
end

module IntAddAction : Action with type t = int and module M = IntAddMonoid = struct
  type t = int
  module M = IntAddMonoid
  let act x a = x + a
end
