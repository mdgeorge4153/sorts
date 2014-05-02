
module type Monad = sig
  type 'a t

  val bind    : 'a t -> ('a -> 'b t) -> 'b t
  val return  : 'a -> 'a t
end

module Utils (M : Monad) : sig
  open M

  val (>>=)   : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)    : 'a t -> 'b t -> 'b t
  val foreach : ~from:int -> ~until:int -> ~init:'a -> (int -> 'a -> 'a t) -> 'a t
end

